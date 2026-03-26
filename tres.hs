import Prelude hiding (Eq, Ord, minimum)


obtenerSaldo :: Int -> Maybe Int
obtenerSaldo = undefined

retirar :: Int -> Int -> Maybe Int
retirar = undefined

procesarRetiro :: Int -> Int -> Maybe Int
procesarRetiro idCuenta cantidad =
    obtenerSaldo idCuenta >>= \saldo ->
    retirar saldo cantidad >>= \nuevoSaldo ->
    Just nuevoSaldo

calcularInteres idCuenta =
    obtenerSaldo idCuenta >>= \saldo ->
    return (saldo + saldo `div` 10)

sumarSaldos :: Maybe Int -> Maybe Int -> Maybe Int
sumarSaldos = \mx my ->    
    mx >>= \x ->
    my >>= \y ->
    return (x + y)



--record de eqDict
data EqDict a = EqDict
  { eq :: a -> a -> Bool
  }

--record de ordict
data OrdDict a = OrdDict
  { leq    :: a -> a -> Bool
  , eqDict :: EqDict a
  }

eqIntDict :: EqDict Int
eqIntDict = EqDict
  { eq = \x y -> x == y
  }

ordIntDict :: OrdDict Int
ordIntDict = OrdDict
  { leq = \x y -> x <= y
  , eqDict = eqIntDict
  }

-- Devuelve el minimo de una lista no vacia
minimum :: OrdDict a -> [a] -> a
minimum = \dict xs ->
  case xs of
    [x] -> x
    (x:xs') ->
      let m = minimum dict xs'
      in if leq dict m x then m else x



-- Verifica si una lista esta ordenada
estaOrdenada :: OrdDict a -> [a] -> Bool
estaOrdenada = \dict xs ->
  case xs of
    [] -> True
    [_] -> True
    (x:y:ys) ->
      leq dict x y && estaOrdenada dict (y:ys)