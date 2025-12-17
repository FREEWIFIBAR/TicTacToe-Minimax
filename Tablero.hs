module Tablero where

type Tablero = [Char]

tableroVacio :: Tablero
tableroVacio = replicate 9 ' '

filas :: Tablero -> [String]
filas f =
  [ take 3 f,
    take 3 (drop 3 f),
    drop 6 f
  ]

movimiento :: Tablero -> Int -> Char -> Tablero
movimiento tabla pos jugador
  | pos < 0 || pos >= length tabla = tabla
  | tabla !! pos /= ' ' = tabla
  | otherwise =
      take pos tabla ++ [jugador] ++ drop (pos + 1) tabla

lineasGanadoras :: [[Int]]
lineasGanadoras =
  [ [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6]
  ]

ganador :: Tablero -> Char
ganador tabla = buscar lineasGanadoras
  where
    buscar [] = ' '
    buscar (l : ls)
      | esLinea l = tabla !! head l
      | otherwise = buscar ls

    esLinea [a, b, c] =
      tabla !! a /= ' '
        && tabla !! a == tabla !! b
        && tabla !! b == tabla !! c

empate :: Tablero -> Bool
empate t = all (/= ' ') t && ganador t == ' '

color :: Char -> String
color 'X' = "\ESC[31mX\ESC[0m" -- Rojo
color 'O' = "\ESC[34mO\ESC[0m" -- Azul
color c = [c]
