module MinMax where

import Tablero (Tablero, empate, ganador, movimiento)

siguiente :: Char -> Char
siguiente 'X' = 'O'
siguiente 'O' = 'X'
siguiente c = c

posicionesVacias :: Tablero -> [Int]
posicionesVacias tabla =
  [i | (i, c) <- zip [0 ..] tabla, c == ' ']

minimax :: Tablero -> Char -> Char -> Int
minimax tabla maquina turno
  | ganador tabla == maquina = 1
  | ganador tabla /= ' ' = -1
  | empate tabla = 0
  | turno == maquina =
      maximum
        [ minimax (movimiento tabla i turno) maquina (siguiente turno)
          | i <- posicionesVacias tabla
        ]
  | otherwise =
      minimum
        [ minimax (movimiento tabla i turno) maquina (siguiente turno)
          | i <- posicionesVacias tabla
        ]

mejorMovimiento :: Tablero -> Char -> Int
mejorMovimiento tabla jugador = snd (maximum (evaluaciones tabla jugador))
  where
    evaluaciones tabla jugador =
      [ (minimax (movimiento tabla i jugador) jugador (siguiente jugador), i)
        | i <- posicionesVacias tabla
      ]
