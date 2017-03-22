module Execute_prog where

import Robo_parser exposing (parse_it)
import Conf_type exposing (..)
import String exposing (..)
import Task exposing (Task, andThen)
import List.Extra as ListE exposing (zip)



getDirLeft facing =
                case facing of
                  "left" -> "down"
                  "right" -> "up"
                  "up" -> "left"
                  "down" -> "right"
                  _  -> ""
getDirRight facing =
                case facing of
                  "left" -> "up"
                  "right" -> "down"
                  "up" -> "right"
                  "down" -> "left"
                  _  -> ""

getRoboForPos robo_pos facing  = case facing of
                               "left" -> {row = robo_pos.row,col =robo_pos.col-1 } 
                               "right" -> {row = robo_pos.row,col =robo_pos.col+1 } 
                               "up" -> {row = robo_pos.row-1,col =robo_pos.col} 
                               "down" -> {row = robo_pos.row +1 ,col =robo_pos.col } 
                               _  -> {row = -1 ,col =-1 } 

                                     


checkCoinPre robo_pos coin_ls = List.length (List.filter (\coin-> coin.row == robo_pos.row && coin.col == robo_pos.col && coin.nCoins > 0) coin_ls) > 0


isInCellCoins robo_pos cellCoins_ls = List.length (List.filter (\coin-> coin.row == robo_pos.row && coin.col == robo_pos.col) cellCoins_ls) > 0 


checkForwRobo robo_pos roboPos_ls = List.length (List.filter (\x->x.row==robo_pos.row && x.col==robo_pos.col) roboPos_ls) > 0



prepareString roboIndex nCoins lineNo stmt =  "ROBO " ++ (roboIndex) ++ "-->\n" ++ "POCKET-COINS --> " ++ (toString nCoins) ++ "\n" ++ "EXECUTING --> Line No <" ++ (toString lineNo) ++ ">\nStatement -->" ++ (stmt) ++ "\n----------------------------------\n"



processCondStmt cond grid r_index allFunc_ls =
         let
          prim_data = grid.primData
          gridDim = prim_data.gridSize
          robo_pos = defaultRobotPos <|ListE.getAt (prim_data.initialRobotPositions) r_index
          robo_pixel = defaultRoboPixel <|ListE.getAt (grid.roboPixel) r_index
          robo_pkt = defaultRoboPkt <|ListE.getAt (grid.roboPocket) r_index
          robo_direc =  defaultRoboDirec <|ListE.getAt prim_data.initialRobotDirections r_index
          coin_info = defaultCellCoins <|ListE.getAt (prim_data.cellCoins) r_index
          coin_pixel = defaultCoinPixel <|ListE.getAt (grid.coinPixel) r_index
                               
          condition = 
            case cond of
                 IsNextWall -> let
                                 robo_position = getRoboForPos robo_pos robo_direc
                               in if(robo_position.row < 0 || robo_position.row >= gridDim.row || robo_position.col < 0 || robo_position.col >= gridDim.col) then True else False
                               
                 IsRobotInNextCell -> let
                                         robo_position = getRoboForPos robo_pos robo_direc
                                      in checkForwRobo robo_position prim_data.initialRobotPositions
                 IsCoinInCurrentCell -> checkCoinPre robo_pos prim_data.cellCoins
                                        
                 IsPocketEmpty -> (robo_pkt<=0)
                 IsError -> True
         in condition





processBlock (bls) r_index allFunc_ls grid =
                             case bls of
                              [] -> (grid,[],r_index,"")
                              (stmt::ls) ->
                               let
                                prim_data = grid.primData
                                gridDim = prim_data.gridSize
                                robo_pos = defaultRobotPos <|ListE.getAt (prim_data.initialRobotPositions) r_index
                                robo_pixel = defaultRoboPixel <|ListE.getAt (grid.roboPixel) r_index
                                robo_pkt = defaultRoboPkt <|ListE.getAt (grid.roboPocket) r_index
                                robo_direc =  defaultRoboDirec <|ListE.getAt prim_data.initialRobotDirections  r_index
                                coin_info = defaultCellCoins <|ListE.getAt (prim_data.cellCoins) r_index
                                coin_pixel = defaultCoinPixel <|ListE.getAt (grid.coinPixel) r_index
                                img_name = case r_index of
                                               0 -> "A"
                                               1 -> "B"
                                               2 -> "C"
                                               3 -> "D"
                                               _ -> ""
                               
                                ans = case stmt of
                                    BasicStmt (Line li) TurnLeft ->
                                                          let
                                                            
                                                            get_dir = getDirLeft robo_direc
                                                            newGrid= {grid |  primData = { prim_data | initialRobotDirections = List.indexedMap (\ind data -> if (ind==r_index) then get_dir else data) <|prim_data.initialRobotDirections}}
                                                            newNcoins = defaultRoboPkt <|ListE.getAt (newGrid.roboPocket) r_index 
                                                            str = prepareString img_name newNcoins li "TurnLeft"
                                                          in (newGrid,ls,r_index,str)
                                                                          
                                    BasicStmt (Line li) TurnRight -> let
                                                          
                                                            get_dir = getDirRight robo_direc
                                                            newGrid= {grid |  primData = {prim_data | initialRobotDirections = List.indexedMap (\ind data -> if (ind==r_index) then get_dir else data) <|prim_data.initialRobotDirections}}
                                                            newNcoins = defaultRoboPkt <|ListE.getAt (newGrid.roboPocket) r_index
                                                            str = prepareString img_name newNcoins li "TurnRight"
                                                                    in (newGrid,ls,r_index,str)
                                    BasicStmt (Line li) GoForward ->
                                                          let
                                                              robo_position = getRoboForPos robo_pos robo_direc
                                                              checkRowCol = if(robo_position.row < 0 || robo_position.row >= gridDim.row || robo_position.col < 0 || robo_position.col >= gridDim.col) then False else True
                                                              checkRoboForw = checkForwRobo robo_position prim_data.initialRobotPositions
                                                              
                                                              newGrid = if (checkRowCol && (not checkRoboForw)) then {grid | primData = {prim_data | initialRobotPositions = List.indexedMap (\ind data -> if (ind==r_index) then robo_position else data) <|prim_data.initialRobotPositions}} else grid
                                                              newNcoins = defaultRoboPkt <|ListE.getAt (newGrid.roboPocket) r_index 
                                                              str = prepareString img_name newNcoins li "GoForward"
                                                          in (newGrid,ls,r_index,str)
                                                           
                                    BasicStmt (Line li) PutCoin -> let
                                                                     inCellCoins= isInCellCoins robo_pos prim_data.cellCoins
                                                                     updateCoin =  List.map (\data -> if(data.row==robo_pos.row && data.col == robo_pos.col) then {data| nCoins = data.nCoins+1}  else data) prim_data.cellCoins
                                                                     newCoin = {row = robo_pos.row, col = robo_pos.col, nCoins = 1}
                                                                     updatePocket= List.indexedMap (\ind data -> if (ind==r_index) then robo_pkt-1 else data) <|grid.roboPocket
                                                                     newGrid = if(robo_pkt>0) then {grid | roboPocket = updatePocket, primData = {prim_data | cellCoins = if(inCellCoins) then updateCoin else (prim_data.cellCoins ++ [newCoin])}} else grid
                                                                     newNcoins = defaultRoboPkt <|ListE.getAt (newGrid.roboPocket) r_index
                                                                     str = prepareString img_name newNcoins li "PutCoin"
                                                                   in (newGrid,ls,r_index,str)
                                    BasicStmt (Line li) PickCoin -> let
                                                                     coinPresent = checkCoinPre robo_pos prim_data.cellCoins
                                                                     updateCoin1 = List.map (\data -> if(data.row==robo_pos.row && data.col == robo_pos.col) then {data|nCoins = data.nCoins-1}  else data) prim_data.cellCoins
                                                                     updateCoin = List.filter (\data -> data.nCoins > 0) updateCoin1
                                                                     updatePocket = List.indexedMap (\ind data -> if (ind==r_index) then robo_pkt+1 else data) (grid.roboPocket)
                                                                     newGrid = if(coinPresent) then {grid | primData = {prim_data | cellCoins = updateCoin},roboPocket =updatePocket} else grid
                                                                     newNcoins = defaultRoboPkt <|ListE.getAt (newGrid.roboPocket) r_index
                                                                     str = prepareString img_name newNcoins li "PickCoin"
                                                                  
                                                                    in (newGrid,ls,r_index,str)
                                                         
                                    BasicStmt (Line li) (Call fname) ->
                                                                      let
                                                                        (Function (Line li) (FuncName name) (Block blls)) = defaultFunc <| ListE.find(\(Function (Line li) (FuncName name) (Block s)) ->name==fname) allFunc_ls
                                                                        newNcoins = defaultRoboPkt <|ListE.getAt (grid.roboPocket) r_index
                                                                        str = prepareString img_name newNcoins li ("Call " ++ fname)
                                                                  
                                                                        new_ls = blls ++ ls
                                                                      in (grid,new_ls,r_index,str)
                                                                                                    
          
                                                                        
                                    ConditionalStmt (Line ifLi) (If cond (Block bls)) (Line elLi) (Else (Block bls1)) ->
                                                                  let
                                                                   condition =  processCondStmt cond grid r_index allFunc_ls
                                                                   newNcoins =  defaultRoboPkt <|ListE.getAt (grid.roboPocket) r_index
                                                                   li = if(condition) then ifLi else elLi
                                                                   stmtStr = if(condition) then "If " else "Else"
                                                                   str = prepareString img_name newNcoins li stmtStr
                                                                  
                                                                   (gri,pr,ind,str1) = if(condition) then (processBlock bls r_index allFunc_ls grid) else (processBlock bls1 r_index allFunc_ls grid)
                                                                  in (gri,pr++ls,ind,str++str1)
                               in ans

processFunction main_fun  allFunc_ls grid  r_index  =
                  case main_fun of
                   (Function (Line li) (FuncName "main") (Block bls)) ->
                                       let
                                        (gr,pr,ind,str)  = processBlock bls r_index allFunc_ls grid
                                        new_str = str ++ ""
                                        blockProg = Block (pr)
                                        funcBlockProg = Function (Line li) (FuncName "main") (blockProg)
                                       in {updGrid = gr,remProg = funcBlockProg,rIndex = r_index,strng = new_str}
                   _  ->  {updGrid = grid,remProg = main_fun, rIndex = r_index, strng = ""}
                                 



updateGrid grid (Functions ls) r_index =
                                case ls of
                                     [] -> {updatedGrid = grid,remainProg =(Functions []),robo_Index=r_index,msg = ""}
                                     _ ->
                                       let prim_data = grid.primData
                                           mainFunc =  defaultFunc <| ListE.find (\(Function (Line li) (FuncName name) (Block bls)) -> name=="main") ls
                                           {updGrid,remProg, rIndex, strng} = processFunction mainFunc ls grid r_index
                                           otherFun_ls = List.filter (\(Function (Line li) (FuncName name) (Block bls)) -> name/="main") ls
                                           functions = [remProg] ++ otherFun_ls 
                                       in {updatedGrid = updGrid,remainProg =(Functions functions),robo_Index=r_index,msg =strng}
                                    
