module AstStmt where

-- Имена переменных
type Var = String

data Operator = Pow   -- Возведение в степень
              | Mult  -- Умножение
              | Div   -- Деление (с остатком)
              | Plus  -- Сложение
              | Minus -- Вычитание
              | Eq    -- Сравнение на равенство
              | Neq   -- Сравнение на неравенство
              | Le    -- Меньше или равно
              | Lt    -- Меньше
              | Ge    -- Больше или равно
              | Gt    -- Больше
              deriving (Eq)

instance Show Operator where
    show Pow = "^"
    show Mult = "*"
    show Div = "/"
    show Plus = "+"
    show Minus = "-"
    show Eq = "=="
    show Neq = "!="
    show Le = "<="
    show Lt = "<"
    show Ge = ">="
    show Gt = ">"

-- Выражения (expressions)
data Expr = Ident Var                -- Идентификатор
          | Num Int                  -- Число
          | BinOp Operator Expr Expr -- Выражение с бинарным оператором
          deriving (Show, Eq)


-- Инструкции (statements)
data Stmt = Ignore Expr               -- Инструкция, которая является выражением (подразумевается, что значение выражения игнорируется)
          | If Expr Stmt (Maybe Stmt) -- Условное выражение. Первый операнд -- условие, второй -- ветка true, третий -- опциональная ветка else
          | While Expr Stmt           -- Цикл с предусловием. Первый операнд -- условие
          | Read Var                  -- Прочитать значение переменной
          | Write Expr                -- Напечатать значение выражения
          | Assign Var Expr           -- Присвоить значение выражения переменной
          | Seq [Stmt]                -- Последовательность инструкций
          deriving (Show, Eq)

-- Абстрактное синтаксическое дерево программы
data Program = Program Stmt -- Программа является инструкцией

printer :: Program -> String
printer (Program s) = printS s where
printS (Ignore x) = printE x ++ ";"
printS (If e1 tr fls) = "if (" ++ printE e1 ++ ") {\n" ++ printS tr ++ "\n} " ++ case fls of
    Just v -> "else {\n" ++ printS v ++ " }\n"
    Nothing -> ""
printS (While expr stm) = "while " ++ printE expr ++ " {\n" ++ printS stm ++ "\n} "
printS (Assign v e) = "var " ++ v ++ " = " ++ printE e ++ ";\n"
printS (Seq (s:se)) = printS s ++ " " ++ printS (Seq se)
printS (Seq []) = ""
printS (Read v) = "read("++v++");\n"
printS (Write e) = "write(" ++ printE e ++ ");\n"

printE (Ident v) = v
printE (Num i) = show i
printE (BinOp op e1 e2) = "(" ++ printE e1 ++ show op ++ printE e2 ++ ")"

expr = Seq [Ignore $ Ident "x", Ignore $ Ident "y"]
expr2 = While (Num 5) (If (Num 6) (Ignore $ Num 6) Nothing)
