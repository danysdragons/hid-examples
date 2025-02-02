import Data.List (intersperse)
import qualified MikeExpr as ME
import ShuntingYard

-- Converting expressions to prefix and postfix forms

data ExprForm = Prefix | Postfix

exprTo :: Show a => ExprForm -> ME.Expr a -> String
exprTo _ (ME.Lit a) = show a
exprTo form (ME.Add e1 e2) = binOp "+" form e1 e2
exprTo form (ME.Mult e1 e2) = binOp "*" form e1 e2

binOp :: Show a => String -> ExprForm -> ME.Expr a -> ME.Expr a -> String
binOp op form e1 e2 = concat $ intersperse " " (args form)
  where
    e1' = exprTo form e1
    e2' = exprTo form e2
    args Prefix = [op, e1', e2']
    args Postfix = [e1', e2', op]

main2 :: IO ()
main2 = mapM_ printExpr strs
  where
    strs = ["42", "12 + 13", "(2+3*3)*5", "1+(1+2)*(2+2*(1+2))+1+3*2"]
    printExpr str = do
      putStrLn $ "Expression: " ++ str
      let expr = convertToExpr str
      putStrLn $ "Prefix form: " ++ exprTo Prefix expr
      putStrLn $ "Postfix form: " ++ exprTo Postfix expr
      putStrLn "-------------"
