module Conf_type (..) where

type Update = Pressed Int | GridUpdate Grid 
type Line = Line Int
type Prog = Functions (List Function)
type Function = Function Line Fname Block
type Fname = FuncName String

type Block = Block (List Statement)
type Statement = BasicStmt Line Command | ConditionalStmt (Line) IfBlock (Line) ElseBlock
type Command = TurnLeft |TurnRight |GoForward |PutCoin | PickCoin| Call String
type IfBlock = If Condition Block
type Condition = IsNextWall |IsRobotInNextCell |IsCoinInCurrentCell| IsPocketEmpty| IsError 
type ElseBlock = Else Block


type alias Conf =
               { gridSize : Pos,
                 cellCoins : List CoinInfo,
                 initialRobotPositions : List Pos,
                 initialRobotDirections : List String,
                 gridCellSize : Int,
                 gridBorderSize : Int,
                 gridCellColor : Color,
                 gridBorderColor : Color,
                 animationSpeed : Int,
                 robotImages : List String,
                 coinImage : String
                 }


type alias Pos =
                { row : Int,
                  col : Int
                }




type alias CoinInfo =
                     { row :Int,
                       col:Int,
                       nCoins : Int
                     }

type alias Color =
                  { r:Int,
                    g:Int,
                    b:Int
                  }


type alias Vector = {x : Float, y:Float}

type alias Grid = {
                    primData : Conf,
                    coinPixel : List Vector,
                    roboPixel : List Vector,
                    roboPocket : List Int,
                    collage_dim : Vector,
                    gridOrigin : Vector
                    
                  }



type alias Status = List RoboProgInfo

type alias RoboProgInfo = {
                           updatedGrid : Grid,
                           remainProg : Prog,
                           robo_Index : Int,
                           msg : String
                          }



type alias Grid_debug = {
                         gridConfig:Grid,
                         debugMsg : String
                        }



defaultFunc = Maybe.withDefault  <| Function (Line 0) (FuncName "") (Block [])
defaultRobotPos = Maybe.withDefault {row=-1,col=-1}
defaultRoboDirec = Maybe.withDefault ""
defaultRoboPixel = Maybe.withDefault  {x=0,y=0}
defaultRoboPkt = Maybe.withDefault  0
defaultCellCoins = Maybe.withDefault {row=-1,col=-1,nCoins=-1}
defaultCoinPixel = Maybe.withDefault  {x=0,y=0}
