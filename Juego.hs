module Juego where

import MinMax (mejorMovimiento, siguiente)
import System.Process (system)
import Tablero (Tablero, color, empate, filas, ganador, movimiento)

mostrarTablero :: Tablero -> IO ()
mostrarTablero tabla = do
  limpiarPantalla
  let [f1, f2, f3] = filas tabla
  putStrLn ""
  putStrLn "      ┌───┬───┬───┐"
  putStrLn $ "      │ " ++ separar f1 ++ " │"
  putStrLn "      ├───┼───┼───┤"
  putStrLn $ "      │ " ++ separar f2 ++ " │"
  putStrLn "      ├───┼───┼───┤"
  putStrLn $ "      │ " ++ separar f3 ++ " │"
  putStrLn "      └───┴───┴───┘"
  putStrLn ""
  where
    separar [a, b, c] = color a ++ " │ " ++ color b ++ " │ " ++ color c

leerEntero :: IO Int
leerEntero = do
  entero <- getLine
  if entero /= "" && all (`elem` "0123456789") entero
    then return (foldl (\n d -> n * 10 + fromEnum d - 48) 0 entero)
    else do
      putStrLn "Entrada inválida. Intenta de nuevo:"
      leerEntero

pedirPosicion :: Char -> IO Int
pedirPosicion jugador = do
  putStrLn $ "Turno de " ++ [jugador] ++ ". Elige una casilla del 1 al 9:"
  pos <- leerEntero
  return (pos - 1)

jugar :: Tablero -> Char -> IO ()
jugar tabla jugador = do
  mostrarTablero tabla
  let g = ganador tabla
  if g /= ' '
    then do
      putStrLn ("¡Gana el jugador " ++ [g] ++ "!")
      pausar
    else
      if empate tabla
        then do
          putStrLn "¡Empate!"
          pausar
        else do
          pos <- pedirPosicion jugador
          let nuevo = movimiento tabla pos jugador
          if nuevo == tabla
            then do
              putStrLn "Movimiento inválido. Pulsa ENTER para continuar..."
              _ <- getLine
              jugar tabla jugador
            else
              jugar nuevo (siguiente jugador)

jugarMaquina :: Tablero -> Char -> Char -> IO ()
jugarMaquina tabla turno jugador = do
  mostrarTablero tabla
  let g = ganador tabla
  if g /= ' '
    then do
      if g == jugador
        then do
          putStrLn "¡Has ganado!"
          pausar
        else do
          putStrLn "¡La máquina gana!"
          pausar
    else
      if empate tabla
        then do
          putStrLn "¡Empate!"
          pausar
        else
          if turno == jugador
            then do
              pos <- pedirPosicion jugador
              let nuevo = movimiento tabla (pos) jugador
              if nuevo == tabla
                then do
                  putStrLn "Movimiento inválido. Pulsa ENTER para continuar..."
                  getLine
                  jugarMaquina tabla turno jugador
                else jugarMaquina nuevo (siguiente turno) jugador
            else do
              putStrLn "Turno de la máquina"
              let pos = mejorMovimiento tabla turno
              let nuevo = movimiento tabla pos turno
              jugarMaquina nuevo (siguiente turno) jugador

limpiarPantalla :: IO ()
limpiarPantalla = do
  _ <- system "clear" -- Linux / Mac
  _ <- system "cls"   -- Windows
  return ()

pausar :: IO ()
pausar = do
  putStrLn "\nPulsa ENTER para salir..."
  _ <- getLine
  return ()
