-- |
-- Tarea 3
--
-- @author: Marin Gallegos Arvin Isaac
-- @author: Pimentel Casillas Fabrizio
--

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




data EqDict a = EqDict
  { eq :: a -> a -> Bool
  }

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

minimum :: OrdDict a -> [a] -> a
minimum = \dict xs ->
  case xs of
    [x] -> x
    (x:xs') ->
      let m = minimum dict xs'
      in if leq dict m x then m else x



estaOrdenada :: OrdDict a -> [a] -> Bool
estaOrdenada = \dict xs ->
  case xs of
    [] -> True
    [_] -> True
    (x:y:ys) ->
      leq dict x y && estaOrdenada dict (y:ys)

data HashableDict a = HashableDict
  { hash    :: a -> Int
  , eqDictH :: EqDict a
  }

insert :: HashableDict k -> k -> v -> HashMap k v -> HashMap k v
insert dict k v m =
  let index = hash dict k
      (before, bucket:after) = splitAt index m
      newBucket = (k, v) : bucket
  in before ++ (newBucket : after)

lookup' :: HashableDict k -> k -> HashMap k v -> Maybe v
lookup' dict k m =
  let index = hash dict k
      bucket = m !! index
      eqF = eq (eqDictH dict)
  in buscar eqF k bucket
  where
    buscar _ _ [] = Nothing
    buscar eqF k ((k', v):xs) =
      if eqF k k'
      then Just v
      else buscar eqF k xs

integerMod :: Int -> Int -> Int
integerMod = mod

hashableIntDict :: HashableDict Int
hashableIntDict = HashableDict
  { hash = \x -> integerMod x 7
  , eqDictH = EqDict { eq = integerEq }
  }


type Bucket k v = [(k, v)]
type HashMap k v = [Bucket k v]

emptyMap :: HashMap k v
emptyMap = replicate 7 []

test :: Maybe String
test =
  let m1 = insert hashableIntDict 42 "cuarenta y dos" emptyMap
  in lookup' hashableIntDict 42 m1
