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