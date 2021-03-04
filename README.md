# HW04

## Дедлайн 23:59 10.03

1. Разработайте конкретный синтаксис для языка L.
   * Результатом должна быть спецификация языка в `pdf` или `markdown` файле -- как вам удобнее.
   * Спецификация должна включать также и лексическую структуру языка.
   * Описывать язык можно при помощи любых средств. Стоит посмотреть, как синтаксис описан в документации ваших трех самых любимых языков программирования.
   * Это задание креативное, вам надо придумать такой конкретный синтаксис, чтобы вам же его потом было удобно парсить.
   * Есть много особенностей, о которых надо будет подумать, например:
      1. Разделять ли операторы языка разделителями или сделать значимыми переносы строк и отступы?
      2. Использовать скобки для группировки блоков кода, ключевые слова begin/end или маркеры конца блока, зависящие от того, в контексте какого оператора мы находимся?
      3. Может вы хотите предоставить синтаксический сахар для облегчения синтаксиса?
      4. Как в вашем языке выглядит целое число? `---1`, `+13` и `+-0` -- корректные числа или нет? Есть ли в языке унарные операторы?
      5. Как в вашем языке выглядит идентификатор? Можно ли в идентификаторах использовать какие-нибудь специальные символы?
      6. Как выглядят ключевые слова?
      7. Как выглядят арифметические выражения?
      8. Обязательна ли ветка else в условном выражении?

2. Реализовать функцию `printer`, печатающую абстрактное синтаксическое дерево программы в ваш конкретный синтаксис. В будущем, композиция парсера и принтера должна быть тождественной функцией `parser . printer === id`.

## Описание абстрактного синтаксиса языка

Есть два варианта: в одном все конструкции языка являются выражениями ([AstExpr.hs]), в другом случае такие конструкции, как условные выражения и циклы, не являются выражениями [AstStmt.hs].

Выберите тот, который вам больше нравится, и для него создайте конкретный синтаксис.

[Запись лекции](https://drive.google.com/file/d/13mX8sydJFpQEroqxfx4kKwGC5O5I_AJA/view?usp=sharing)



