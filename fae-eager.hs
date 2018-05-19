type Identifier = String
data Value = NumV Int
           | FunV (Value -> Value)

type Env = [(Identifier, Value)]
data FAE = Num Int
         | Add FAE FAE
         | Id Identifier
         | Fun Identifier FAE
         | App FAE FAE

interp :: FAE -> Env -> Value
interp (Num n) env = NumV n
interp (Add lhs rhs) env = addNumbers (interp lhs env) (interp rhs env)
interp (Id i) env = lookUp i env
interp (Fun arg body) env =
    FunV (\ val -> val `seq` interp body (extend env arg val))
interp (App funExpr argExpr) env =
    let FunV funVal = interp funExpr env
        argVal = interp argExpr env
    in (funVal argVal)

addNumbers :: Value -> Value -> Value
addNumbers (NumV n1) (NumV n2) = NumV (n1 + n2)

lookUp :: Identifier -> Env -> Value
lookUp var ((i,v):r)
    | (var == i) = v
    | otherwise = lookUp var r

extend :: Env -> Identifier -> Value -> Env
extend env i v = (i,v):env
