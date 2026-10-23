package foodtruck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FoodTruckModelSpec extends AnyFlatSpec with Matchers {

  "ShiftProcessor" should "коректно розраховувати стан зміни для успішного сценарію" in {
    val events = List(
      Event.InitialCash(100.0),                      // Внесення розміну: 100 грн
      Event.Sale(50.0, PaymentMethod.Cash),           // Продаж готівка: +50
      Event.Sale(150.0, PaymentMethod.Terminal),     // Продаж термінал: +150
      Event.Expense(20.0, "Купівля льоду"),          // Витрата готівка: -20
      Event.Encashment(80.0)                         // Інкасація: -80
    )

    val result = ShiftProcessor.processEvents(events)

    result.isRight shouldBe true
    val state = result.toOption.get

    // Перевірка поточної суми готівки в касі: 100 + 50 - 20 - 80 = 50
    state.cashInDrawer shouldBe BigDecimal(50.0)

    // Перевірка виручки за типом оплати
    state.totalCashRevenue shouldBe BigDecimal(50.0)
    state.totalTerminalRevenue shouldBe BigDecimal(150.0)
    state.totalRevenue shouldBe BigDecimal(200.0)

    // Перевірка середнього чека: (50 + 150) / 2 = 100
    state.averageCheck shouldBe BigDecimal(100.0)
  }

  it should "забороняти інкасацію, якщо в касі недостатньо готівки" in {
    val events = List(
      Event.InitialCash(50.0),
      Event.Encashment(100.0) // Спроба вилучити 100 грн при 50 грн у касі
    )

    val result = ShiftProcessor.processEvents(events)

    result.isLeft shouldBe true
    result.left.toOption.get shouldBe "Недостатньо готівки в касі для інкасації"
  }

  it should "забороняти службову видачу (витрату), якщо в касі недостатньо готівки" in {
    val events = List(
      Event.InitialCash(30.0),
      Event.Expense(50.0, "Стаканчики") // Спроба витратити 50 грн при 30 грн у касі
    )

    val result = ShiftProcessor.processEvents(events)

    result.isLeft shouldBe true
    result.left.toOption.get shouldBe "Недостатньо готівки в касі для службової видачі"
  }
}