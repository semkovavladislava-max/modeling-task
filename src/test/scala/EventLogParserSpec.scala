package foodtruck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Paths}

class EventLogParserSpec extends AnyFlatSpec with Matchers {

  "EventLogParser" should "коректно парсити та згортати валідний сценарій зміни з CSV-рядків" in {
    val csvLines = List(
      "event,p1,p2,p3,p4",
      "FLOAT_IN,1000.00,,,",
      "SALE,250.00,CASH,,",
      "SALE,350.00,CARD,,",
      "SUPPLY_PURCHASE,150.00,,,",
      "COLLECTION,600.00,,,"
    )

    val report = EventLogParser.processLines(csvLines)

    report.totalDataRows shouldBe 5
    report.validParsedCount shouldBe 5
    report.parseErrors shouldBe empty
    report.rejectedByInvariant shouldBe empty

    // Перевірка стану та аналітики:
    // Готівка: 1000 + 250 - 150 - 600 = 500
    ShiftProcessor.currentCash(report.finalState) shouldBe BigDecimal("500.00")
    // Виручка: (250 готівка, 350 термінал, 600 разом)
    ShiftProcessor.revenueBreakdown(report.finalState) shouldBe (
      BigDecimal("250.00"),
      BigDecimal("350.00"),
      BigDecimal("600.00")
    )
    // Середній чек: 600 / 2 = 300
    ShiftProcessor.averageOrderCheck(report.finalState) shouldBe BigDecimal("300.00")
  }

  it should "відносити синтаксично невалідні рядки у ліву рейку ParseError без винятків" in {
    val dirtyLines = List(
      "event,p1,p2,p3,p4",
      "NOT_AN_EVENT,97.59,CASH,,",   // UnknownEvent
      ",100.00,,,",                   // EmptyEvent
      "SALE,-50.00,CASH,,",           // InvalidMoney (<= 0)
      "SALE,abc,CARD,,",              // InvalidMoney (malformed)
      "SALE,120.00,CRYPTO,,",         // InvalidPaymentChannel
      "SALE,100.00,CASH"              // ColumnCountError (3 замість 5)
    )

    val report = EventLogParser.processLines(dirtyLines)

    report.validParsedCount shouldBe 0
    report.parseErrors should have size 6
    report.parseErrors(0) shouldBe a[ParseError.UnknownEvent]
    report.parseErrors(1) shouldBe a[ParseError.EmptyEvent]
    report.parseErrors(2) shouldBe a[ParseError.InvalidMoney]
    report.parseErrors(3) shouldBe a[ParseError.InvalidMoney]
    report.parseErrors(4) shouldBe a[ParseError.InvalidPaymentChannel]
    report.parseErrors(5) shouldBe a[ParseError.ColumnCountError]
  }

  it should "відхиляти інкасацію або закупівлю при нестачі готівки, зберігаючи решту зміни" in {
    val lines = List(
      "event,p1,p2,p3,p4",
      "FLOAT_IN,200.00,,,",
      "COLLECTION,500.00,,,",       // Порушення інваріанту: 500 > 200
      "SUPPLY_PURCHASE,250.00,,,",  // Порушення інваріанту: 250 > 200
      "SALE,100.00,CASH,,"         // Валідна операція після невдалих спроб
    )

    val report = EventLogParser.processLines(lines)

    report.validParsedCount shouldBe 4
    report.parseErrors shouldBe empty
    report.rejectedByInvariant should have size 2
    report.rejectedByInvariant.head._2 shouldBe BusinessError.InsufficientCash(BigDecimal("500.00"), BigDecimal("200.00"))
    report.rejectedByInvariant(1)._2 shouldBe BusinessError.InsufficientCash(BigDecimal("250.00"), BigDecimal("200.00"))

    // Стан зміни залишився консистентним: 200 + 100 = 300
    ShiftProcessor.currentCash(report.finalState) shouldBe BigDecimal("300.00")
  }

  it should "успішно обробляти повний згенерований журнал на 500 000 рядків Варіанта 7" in {
    val candidatePaths = List(
      "course_repo/n04_state_event_logs/generated/variant_07_foodtruck_cash_register.csv",
      "src/test/resources/variant_07_foodtruck_cash_register.csv"
    )

    val existingPath = candidatePaths.find(p => Files.exists(Paths.get(p)))
    assume(existingPath.isDefined, "Згенерований CSV-файл не знайдено для інтеграційного тесту")

    val result = EventLogParser.processCsvFile(existingPath.get)
    result.isRight shouldBe true

    result.foreach { report =>
      report.totalDataRows shouldBe 500000
      report.validParsedCount shouldBe 475047
      report.parseErrors should have size 24953
      ShiftProcessor.currentCash(report.finalState) should be >= BigDecimal(0)
    }
  }
}