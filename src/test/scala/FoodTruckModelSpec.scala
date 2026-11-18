package foodtruck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FoodTruckModelSpec extends AnyFlatSpec with Matchers {

  "ShiftProcessor" should "коректно розраховувати стан зміни для успішного сценарію" in {
    val events = List(
      Event.InitialCash(100.0),                   // Внесення розміну: 100 грн
      Event.Sale(50.0, PaymentMethod.Cash),       // Продаж готівка: +50
      Event.Sale(150.0, PaymentMethod.Terminal),  // Продаж термінал: +150
      Event.Expense(20.0, "Купівля льоду"),       // Витрата готівка: -20
      Event.Encashment(80.0)                      // Інкассація: -80
    )

    val result = ShiftProcessor.processEvents(events)

    result should matchPattern { case Right(_) => }
    result.foreach { state =>
      // Поточна сума готівки в касі: 100 + 50 - 20 - 80 = 50
      ShiftProcessor.currentCash(state) shouldBe BigDecimal(50.0)

      // Виручка: (готівка, термінал, загальна)
      ShiftProcessor.revenueBreakdown(state) shouldBe (
        BigDecimal(50.0),
        BigDecimal(150.0),
        BigDecimal(200.0)
      )

      // Середній чек: (50 + 150) / 2 = 100
      ShiftProcessor.averageOrderCheck(state) shouldBe BigDecimal(100.0)
    }
  }

  it should "забороняти інкасацію, якщо в касі недостатньо готівки, і не змінювати стан" in {
    val initialState = ShiftState(cashInDrawer = BigDecimal(50.0))
    val result = ShiftProcessor.applyEvent(initialState, Event.Encashment(100.0))

    result shouldBe Left(BusinessError.InsufficientCash(requested = 100.0, available = 50.0))
  }

  it should "забороняти службову видачу (витрату), якщо в касі недостатньо готівки" in {
    val events = List(
      Event.InitialCash(30.0),
      Event.Expense(50.0, "Стаканчики")
    )

    val result = ShiftProcessor.processEvents(events)

    result shouldBe Left(BusinessError.InsufficientCash(requested = 50.0, available = 30.0))
  }

  it should "безпечно повертати 0 для середнього чека, якщо продажів ще не було" in {
    val emptyShift = ShiftState(cashInDrawer = BigDecimal(100.0))

    ShiftProcessor.averageOrderCheck(emptyShift) shouldBe BigDecimal(0)
  }

  it should "відхиляти події з некоректною (недодатною) сумою" in {
    val state = ShiftState(cashInDrawer = BigDecimal(100.0))

    ShiftProcessor.applyEvent(state, Event.Sale(-10.0, PaymentMethod.Cash)) shouldBe
      Left(BusinessError.InvalidAmount(-10.0))
  }
}