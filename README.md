# Варіант 7: Касова зміна фудтрака (`foodtruck_cash_register`)

Лабораторні роботи з функціонального програмування:
* **n03 (Моделювання стану):** незмінний стан зміни (`ShiftState`), алгебраїчні типи даних (`Event`, `PaymentMethod`, `BusinessError`), чиста функція переходу (`applyEvent`) та аналітичні селектори.
* **n04 (Журнали подій та Railway-Oriented Programming):** парсер великих CSV-журналів (`EventLogParser`) без винятків (`Either[ParseError, Event]`), валідація полів через Smart Constructors та згортання журналу подій.

## Структура проєкту
* `src/main/scala/FoodTruckModel.scala` — доменна модель, інваріанти каси та аналітика.
* `src/main/scala/EventLogParser.scala` — CSV-парсер, типізовані помилки розбору та агрегатор звіту зміни.
* `src/test/scala/FoodTruckModelSpec.scala` — модульні тести доменної моделі та бізнес-інваріантів.
* `src/test/scala/EventLogParserSpec.scala` — тести парсингу, обробки невалідних рядків і повного журналу на 500 000 рядків.

## Підготовка тестових даних (n04)
Великий CSV-файл не зберігається в Git-репозиторії (ігнорується через `.gitignore`). Щоб згенерувати його локально:

1. Згенеруйте журнал для Варіанта 7 за допомогою скрипту з репозиторію курсу:
   ```bash
   python3 generate_event_logs.py --variant 7 --rows 500000