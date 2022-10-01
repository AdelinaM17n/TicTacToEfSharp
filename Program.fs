﻿type boardVal =
    | Empty
    | TeamOne
    | TeamTwo
    
type boardPos = {
    x : int
    y : int
}

type dropResult =
    | Success
    | Fail
    
let boardArray : boardVal[,] = Array2D.init 3 3 (fun y x -> Empty)

let checkForWinVertically team pos =
    (
        if pos.y >= 0 && pos.y < 2
              then
                  if boardArray[pos.y+1, pos.x] = team
                  then
                      if pos.y+2 = 2 && boardArray[pos.y+2, pos.x] = team
                      then 2 else 1
                  else 0
              else 0
    )
    +
    (
        if pos.y <= 2 && pos.y > 0
              then
                  if boardArray[pos.y-1, pos.x] = team
                  then
                      if pos.y-2 = 0 && boardArray[pos.y-2, pos.x] = team
                      then 2 else 1
                  else 0
              else 0
    ) >= 2
    
let checkForWinHorizontally team pos =
    (
        if pos.x >= 0 && pos.x < 2
              then
                  if boardArray[pos.y, pos.x+1] = team
                  then
                      if pos.x+2 = 2 && boardArray[pos.y, pos.x+2] = team
                      then 2 else 1
                  else 0
              else 0
    )
    +
    (
        if pos.x <= 2 && pos.x > 0
              then
                  if boardArray[pos.y, pos.x-1] = team
                  then
                      if pos.x-2 = 0 && boardArray[pos.y, pos.x-2] = team
                      then 2 else 1
                  else 0
              else 0
    ) >= 2
    
let rec checkForWinDiagonally team pos y x turn =
    (
        if (pos.y+y >= 0 && pos.y+y <= 2) && (pos.x+x >= 0 && pos.x+x <= 2)
              then
                  if boardArray[pos.y+y, pos.x+x] = team
                  then
                      if ((pos.x+(x+x) <= 2 && pos.y+(y+y) <= 2) && (pos.x+(x+x) >= 0 && pos.y+(y+y) >= 0))
                         && boardArray[pos.y+(y+y), pos.x+(x+x)] = team
                      then 2 else 1
                  else 0
              else 0
    )
    +
    (
        if (pos.y-y >= 0 && pos.y-y <= 2) && (pos.x-x >= 0 && pos.x-x <= 2)
              then
                  if boardArray[pos.y-x, pos.x-x] = team
                  then
                      if ((pos.x-(x+x) <= 2 && pos.y-(y+y) <= 2) && (pos.x-(x+x) >= 0 && pos.y-(y+y) >= 0))
                         && boardArray[pos.y-(y+y), pos.x-(x+x)] = team
                      then 2 else 1
                  else 0
              else 0
    ) |> (
        fun result ->
            if result >= 2 then
                true
            elif turn = false then
                checkForWinDiagonally team pos -1 x true
            else
                false
     )
    
let checkForWin = 7 + 7


let dropPiece team pos =
    match boardArray[pos.y,pos.x] with
    | Empty ->
        boardArray[pos.y,pos.x] <- team
        // checkForWin
        Success
    | TeamOne | TeamTwo -> Fail
