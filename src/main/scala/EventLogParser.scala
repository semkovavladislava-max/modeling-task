package foodtruck

import scala.io.Source
import scala.util.Using

// 1. ADT помилок парсингу (рівень зовнішнього CSV-контракту)
enum ParseError(val rowIndex: Int):
  case ColumnCountError(override val rowIndex: Int, expected: Int, actual: Int) extends ParseError(rowIndex)
  case EmptyEvent(override val rowIndex: Int) extends ParseError(rowIndex)
  case UnknownEvent(override val rowIndex: Int, code: String) extends ParseError(rowIndex)
  case InvalidMoney(override val rowIndex: Int, field: String, raw: String, reason: String) extends ParseError(rowIndex)
  case InvalidPaymentChannel(override val rowIndex: Int, raw: String) extends ParseError(rowIndex)

// Підсумковий звіт обробки журналу
case class LogProcessingReport(
                                totalDataRows: Int,
                                validParsedCount: Int,
                                parseErrors: List[ParseError],
                                rejectedByInvariant: List[(Event, BusinessError)],
                                finalState: ShiftState
                              )

object EventLogParser:

  private val ExpectedColumns = 5 // event,p1,p2,p3,p4
  private val HeaderPrefix = "event,p1,p2,p3,p4"

  // 2. Smart Constructors (без побічних ефектів та винятків)
  def parsePositiveMoney(raw: String, field: String, rowIndex: Int): Either[ParseError, BigDecimal] =
    val trimmed = raw.trim
    scala.util.Try(BigDecimal(trimmed)).toOption match
      case None =>
        Left(ParseError.InvalidMoney(rowIndex, field, raw, "Not a valid decimal number"))
      case Some(amount) if amount <= 0 =>
        Left(ParseError.InvalidMoney(rowIndex, field, raw, "Amount must be strictly positive"))
      case Some(amount) =>
        Right(amount)

  def parsePaymentChannel(raw: String, rowIndex: Int): Either[ParseError, PaymentMethod] =
    raw.trim match
      case "CASH" => Right(PaymentMethod.Cash)
      case "CARD" => Right(PaymentMethod.Terminal)
      case other  => Left(ParseError.InvalidPaymentChannel(rowIndex, other))

  // 3. Railway-парсер одного рядка CSV
  def parseLine(rawLine: String, rowIndex: Int): Either[ParseError, Event] =
    val cols = rawLine.split(",", -1)
    if cols.length != ExpectedColumns then
      Left(ParseError.ColumnCountError(rowIndex, ExpectedColumns, cols.length))
    else
      val eventCode = cols(0).trim
      val p1 = cols(1)
      val p2 = cols(2)

      if eventCode.isEmpty then Left(ParseError.EmptyEvent(rowIndex))
      else
        eventCode match
          case "FLOAT_IN" =>
            parsePositiveMoney(p1, "p1", rowIndex).map(Event.InitialCash.apply)

          case "SALE" =>
            for
              amount <- parsePositiveMoney(p1, "p1", rowIndex)
              method <- parsePaymentChannel(p2, rowIndex)
            yield Event.Sale(amount, method)

          case "COLLECTION" =>
            parsePositiveMoney(p1, "p1", rowIndex).map(Event.Encashment.apply)

          case "SUPPLY_PURCHASE" =>
            parsePositiveMoney(p1, "p1", rowIndex).map(amount =>
              Event.Expense(amount, "Supply purchase")
            )

          case other =>
            Left(ParseError.UnknownEvent(rowIndex, other))

  // 4. Пакетний імпорт та згортання журналу
  def processLines(lines: Iterable[String]): LogProcessingReport =
    val dataLines = lines.dropWhile(_.trim.equalsIgnoreCase(HeaderPrefix)).toList

    val (parseErrors, validEvents) = dataLines.zipWithIndex.partitionMap { case (line, idx) =>
      parseLine(line, idx + 2) // +2 бо 1-й рядок заголовок, індексація з 1
    }

    // Безпечне згортання: якщо подія порушує інваріант каси, фіксуємо відмову і зберігаємо поточний стан
    val (finalState, rejectedReversed) =
      validEvents.foldLeft((ShiftState(), List.empty[(Event, BusinessError)])) {
        case ((state, rejected), event) =>
          ShiftProcessor.applyEvent(state, event) match
            case Right(nextState) => (nextState, rejected)
            case Left(bizError)   => (state, (event, bizError) :: rejected)
      }

    LogProcessingReport(
      totalDataRows = dataLines.size,
      validParsedCount = validEvents.size,
      parseErrors = parseErrors,
      rejectedByInvariant = rejectedReversed.reverse,
      finalState = finalState
    )

  // Читання файлу з автоматичним закриттям ресурсу (Using)
  def processCsvFile(filePath: String): Either[String, LogProcessingReport] =
    Using(Source.fromFile(filePath, "UTF-8")) { source =>
      processLines(source.getLines().toList)
    }.toEither.left.map(_.getMessage)