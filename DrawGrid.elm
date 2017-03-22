module DrawGrid where

import Text 
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List.Extra as ListE exposing (zip)
import Conf_type exposing (..)


draw_line ax ay bx by col=
 traced (solid (rgb col.r col.g col.b)) <| path [(toFloat ax,toFloat ay),(toFloat bx,toFloat by)]

hor_lines cellsize rows cols origin col = List.foldl (\y acc-> (draw_line (0+origin.x) (y+origin.y) (cols*cellsize + origin.x) (y+origin.y) col ) :: acc) []  (List.map (\x->x*cellsize) [0..rows])

ver_lines cellsize rows cols origin col = List.foldl (\x acc -> (draw_line (x+origin.x) (0+origin.y) (x+origin.x) (rows*cellsize + origin.y) col) :: acc) []  (List.map (\x->x*cellsize) [0..cols])


drawGridLines cell_sz rows cols origin col=
                        (hor_lines cell_sz rows cols origin col) ++ (ver_lines cell_sz rows cols origin col)

drawCoinImage cellSize ({nCoins},{x,y}) = toString nCoins |> Text.fromString |> (Text.color <|rgb 255 215 0) |> Text.height (toFloat <|cellSize//4) |> Text.bold|> text |> move (toFloat x,toFloat y)

drawRoboImage cellSize (imgSrc,{x,y},angle) =  fittedImage (cellSize - 30) (cellSize- 30) imgSrc |> toForm  |>  move (toFloat x,toFloat y) |> rotate angle


draw_collage {x,y} {r,g,b} diagram =  color (rgb r g b) <|collage x y diagram 
 

getRotation dir = case dir of
                   "up" -> degrees 180
                   "down"-> degrees 0
                   "left" ->degrees 270
                   "right" -> degrees 90
                   _ -> degrees 0


drawGrid1 grid =
                 let
                    origin = grid.gridOrigin
                    prim_data = grid.primData
                    rows = prim_data.gridSize.row
                    cols = prim_data.gridSize.col
                    coll_sz = grid.collage_dim
                    cellSize = prim_data.gridCellSize
                    face_list = List.map getRotation prim_data.initialRobotDirections
                    roboImage = List.map (drawRoboImage cellSize) <| ListE.zip3 prim_data.robotImages grid.roboPixel face_list
                    coinImage = List.map (drawCoinImage cellSize) <| zip  prim_data.cellCoins grid.coinPixel 
                    gridLines = drawGridLines cellSize rows cols origin prim_data.gridBorderColor
                    whole_grid = gridLines ++ roboImage ++ coinImage
                    collage_w_diag = draw_collage coll_sz prim_data.gridCellColor whole_grid
                 in collage_w_diag


getPixel startPos cellSize coord = let
                                    yPixel = startPos.y - coord.row *cellSize
                                    xPixel = startPos.x + coord.col * cellSize
                                   in {x=xPixel,y=yPixel}





drawGrid grid =
                let
                  origin = grid.gridOrigin
                  prim_data = grid.primData
                  rows = prim_data.gridSize.row
                  cols = prim_data.gridSize.col
                  cellSize = prim_data.gridCellSize
                  robo_startPosition = { x= origin.x + cellSize//2 , y = origin.y + (rows * cellSize - 63)}
                  coin_startPosition = { x= origin.x + cellSize//2 , y = origin.y + (rows * cellSize - cellSize//7)}
                      
                  updated_grid = {grid | coinPixel = List.map (getPixel coin_startPosition cellSize) prim_data.cellCoins,
                        roboPixel = List.map (getPixel robo_startPosition cellSize) prim_data.initialRobotPositions
                       }
                in drawGrid1 updated_grid 