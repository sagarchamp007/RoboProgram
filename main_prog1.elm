import Robo_parser exposing (parse_it)
import Conf_type exposing (..)
import Execute_prog exposing (updateGrid)

import String exposing (..)
import Html exposing (Html, Attribute, text,div,textarea,button,form,br,p)
import Json.Decode as Json exposing ((:=))
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue,onClick)
import Http
import Html exposing (Html)
import Task exposing (Task, andThen)
import Graphics.Element exposing (..)
import List.Extra as ListE exposing (zip)
import Json.Decode.Extra as JsonE exposing ((|:))
import DrawGrid exposing (drawGrid)
import Signal.Extra as SignalE
import Keyboard exposing (presses)


emptyConf =  { gridSize = {row=0,col=0},
                 cellCoins = [],
                 initialRobotPositions = [],
                 initialRobotDirections = [],
                 gridCellSize = 0,
                 gridBorderSize = 0,
                 gridCellColor = {r = 0,g =0,b =0},
                 gridBorderColor = {r = 0,g =0,b =0},
                 animationSpeed = 0,
                 robotImages = [],
                 coinImage = ""
                }

emptyGrid = {
              primData = emptyConf,
              coinPixel = [],
              roboPixel = [],
              roboPocket = [],
              collage_dim = {x=0,y=0},
              gridOrigin = {x=0,y=0}

             }



-- VIEW
parse_view string string1 parse_res grid_debug = let 
                       input_view = div []
                                   [stringInput string
                                    ,drawButton string
                                   ]
                       parse_out = case parse_res of
                                     Err err -> div[myTextStyle] <|List.map (\x->div [] [text x])(String.split "\n" err)
                                     Ok value -> div[myTextStyle] [div [][text "Compiled SuccesFully"]]
                       debug_d = debugOut grid_debug.debugMsg
                       grid_d =  div [myGridStyle] [Html.fromElement <| drawGrid grid_debug.gridConfig]
                                           in div [mydivStyle] [input_view,parse_out,debug_d,grid_d]


debugOut string =
  textarea
    [ placeholder "Debug Info here"
    , value string
    , mydebugStyle
    ]
    []




stringInput : String -> Html
stringInput string =
  textarea
    [ placeholder "Input Code here"
    , value string
    , on "input" targetValue (Signal.message prog_input.address)
    , myStyle
    ]
    []

drawButton string =
  button
  [onClick result.address string,
  myBtStyle
  ]
  [text "Submit"]



myStyle : Attribute
myStyle =
  style
  [ ("width", "30%")
  , ("height", "400px")
  , ("padding", "10px 0")
  , ("font-size", "1.5em")
  ,("background","lightgrey")
  ,("top","0px")
  ,("left","0px")
  ]
  
myBtStyle =
  style
  [("position","absolute"),
   ("top","420px")
  ,("left","200px")
  ]

myTextStyle =
  style
  [("position","absolute"),
   ("top","480px")
  ,("left","200px")
  ]

myGridStyle =
 style
  [("width","50%"),
   ("position","absolute"),
   ("top","0px")
  ,("left","750px")
 
  ]

mydivStyle =
 style
 [("width","100%"),
 ("clear","both")
 ]

mydebugStyle =
 style
  [ ("width", "25%")
  ,("position","absolute")
  , ("height", "400px")
  , ("padding", "10px 0")
  , ("font-size", "1.5em")
  ,("background","lightgrey")
  ,("top","0px")
  ,("left","400px")
  ]
  





init_grid conf_data =
                     let
                       rows = conf_data.gridSize.row
                       cols = conf_data.gridSize.col
                       cellSize = conf_data.gridCellSize
                       coll_dim = {x=cellSize*rows,y=cellSize*cols}
                       origin = {x= -coll_dim.x//2,y=-coll_dim.y//2}
                       
                       
                     in { primData = conf_data,
                        coinPixel = [],
                        roboPixel = [],
                        roboPocket = List.repeat (List.length conf_data.initialRobotPositions) 0,
                        collage_dim = coll_dim,
                        gridOrigin = origin 
                       }


callupdateGrid l_info p_info = updateGrid l_info.updatedGrid p_info.remainProg p_info.robo_Index

getStatus (code) status initInfo grid prog =
                              case status of
                                   [] ->
                                    case prog of
                                      Err err -> []
                                      Ok value ->
                                           let
                                             init_Status = List.repeat (List.length grid.roboPocket) {updatedGrid = grid,remainProg = value, robo_Index = 0,msg = ""}
                                            in List.indexedMap (\ind st-> {st| robo_Index = ind}) init_Status
                                   _  ->
                                       case code of
                                          40 ->
                                           let latestInfo = Maybe.withDefault initInfo<|ListE.last status
                                           in List.drop 1 <|List.scanl (\p_info  l_info-> (callupdateGrid l_info p_info)) latestInfo status 
                                          _ -> status
                               

                               
                               

init_info grid prog =
                     case prog of
                      Err err -> {updatedGrid= grid,remainProg = Functions [], robo_Index = 0,msg = ""}
                      Ok value -> {updatedGrid= grid,remainProg = value, robo_Index = 0,msg = ""}



getGrid status initGrid =
                       (Maybe.withDefault initGrid <|ListE.last <|List.map .updatedGrid status) 

-- SIGNALS
prog_input = 
    Signal.mailbox ""
  
result = 
   Signal.mailbox ""

parse_out =
   Signal.mailbox (Err "parse_error")


config =
  Signal.mailbox emptyConf

grid_mail =
  Signal.mailbox {gridConfig=emptyGrid,debugMsg = ""}  


port requests: Signal (Task x ())
port requests =  Signal.map parse_it result.signal |> Signal.map (\res -> Signal.send parse_out.address res)
            

port new_status : Signal (Task x ())
port new_status =
            let
              initialGrid = Signal.map (init_grid) config.signal
              initInfo    =  Signal.map2 (init_info) initialGrid parse_out.signal
              pressed = presses
              all_signal = SignalE.zip4 parse_out.signal initialGrid pressed initInfo
              all_state_ls =  Signal.foldp (\(prog,grid,code,initInfo) status -> getStatus code status initInfo grid prog) [] all_signal
              concatRoboMsgs = Signal.map (\x-> List.map .msg x) all_state_ls|> Signal.map (\x-> String.concat x)
            in Signal.map2 (getGrid) all_state_ls initialGrid |> Signal.map2 (\str gr -> Signal.send grid_mail.address {gridConfig = gr,debugMsg = str}) concatRoboMsgs


                 


--send some markdown to our config mailbox
report : Conf -> Task x ()
report markdown =
  Signal.send config.address markdown


--get the config *and then* send the result to our mailbox
port fetchConfig : Task Http.Error ()
port fetchConfig =
  Http.get decode_conf configUrl `Task.andThen` report


--the URL of the CONFIG that we desire
configUrl : String
configUrl ="https://raw.githubusercontent.com/sagarchamp007/elm-stuff/master/conf.json"



             
decode_conf = Json.succeed Conf
                 |: ("gridSize" := Json.object2 Pos ("row":= Json.int) ("col":= Json.int))
                 |: ("cellCoins" := Json.list (Json.object3 CoinInfo ("row":= Json.int) ("col":= Json.int) ("ncoins" := Json.int)))
                 |: ("initRoboPos" := Json.list (Json.object2 Pos ("row":= Json.int) ("col":= Json.int)))
                 |: ("initRoboDir" := Json.list (Json.string))
                 |: ("gridCellSize" := Json.int)
                 |: ("gridBorderSize" := Json.int)
                 |: ("gridCellCol" := Json.object3 Color ("r":= Json.int) ("g" := Json.int) ("b":= Json.int))
                 |: ("gridBordCol" := Json.object3 Color ("r":= Json.int) ("g" := Json.int) ("b":= Json.int))
                 |: ("animSpeed":= Json.int)
                 |: ("roboImages":= Json.list Json.string)
                 |: ("coinImage":= Json.string)   



main =
       let
          a = []
       in Signal.map4 (parse_view) prog_input.signal result.signal parse_out.signal grid_mail.signal
      




