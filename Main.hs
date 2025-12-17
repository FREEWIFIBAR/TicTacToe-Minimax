module Main where

import Juego (jugar, jugarMaquina, limpiarPantalla)
import Tablero (tableroVacio)

main :: IO ()
main = menu

menu :: IO ()
menu = do
  limpiarPantalla
  putStrLn "\ESC[36m============================\ESC[0m"
  putStrLn "\ESC[36m       TRES EN RAYA         \ESC[0m"
  putStrLn "\ESC[36m============================\ESC[0m"
  putStrLn "\ESC[32m1. Jugador vs Jugador\ESC[0m"
  putStrLn "\ESC[33m2. Jugador vs Máquina\ESC[0m"
  putStrLn "\ESC[31m3. Salir\ESC[0m"
  putStrLn "Elige una opción:"

  opcion <- getLine
  case opcion of
    "1" -> partidaJugadores
    "2" -> partidaContraMaquina
    "3" -> putStrLn "¡Hasta luego!"
    _ -> do
      putStrLn "Opción no válida. Pulsa ENTER..."
      _ <- getLine
      menu

partidaJugadores :: IO ()
partidaJugadores = do
  jugar tableroVacio 'X'
  menu

partidaContraMaquina :: IO ()
partidaContraMaquina = do
  putStrLn "¿Quieres jugar como 'X' o 'O'?"
  ficha <- getLine
  let jugador = if ficha == "O" || ficha == "o" then 'O' else 'X'
  jugarMaquina tableroVacio 'X' jugador
  menu
