![alt text](https://github.com/sagarchamp007/RoboProgram/blob/master/ezgif.com-resize.gif?raw=true)





PREREQUISITS--
-- The ROBOT_CONFIGURATION file is fetched from url "https://raw.githubusercontent.com/sagarchamp007/elm-stuff/master/conf.json" using http get requests in main_prog1.elm
______________________________________________
draft CFG for the RobotProgram
______________________________________________
RobotProgram = otherFunctions mainFunction otherFunctions
block = \n { \n stmts \n } \n 
stmts = stmt \n stmts
stmt = basicstmt | conditionalstmt
basicstmt = TurnLeft | TurnRight | GoForward | PutCoin | PickCoin | Call functionName 
conditionalstmt = if (condition) block else block
condition = IsNextWall | IsRobotInNextCell | IsCoinInCurrentCell | IsPocketEmpty | IsError
functionName = alphabet functionName | alphabet
otherFunctions = otherFunction otherFunctions | <empty>
otherFunction = def functionName () block 
mainFunction = def main () block 

_________________________________________________

SAMPLE PROGRAM-
def sag()
{
PickCoin
if(IsCoinInCurrentCell)
{
PickCoin
}
else
{
TurnLeft
TurnRight
}
GoForward
PutCoin
Call sag

}

def main()
{
Call sag
}
__________________________________________________

REQUIREMENTS-
- Elm 0.16.0 installed
- other required packages are in package.json


FILES-
- main_prog1.elm -- Main program to be compiled.
--Conf_type.elm -- code having types defined.
--DrawGrid.elm -- module for drawing grid.
--Robo_parser.elm -- module for parsing robot input program.
--Execute_prog.elm -- module for executing input program by user.


RUN--
elm-reactor - <start the elm server>
elm-make main_prog1.elm --output=<file_name>.html
  It will generate (<file_name>.html) file. This file can be viewed on localhost.

Instructions For using app--
- Try To run it on Firefox.
- Program is given as input in text-area tagged "input code here".
- SUBMIT button is clicked to check for errors if any in the input code.
- KEEYBOARD D KEY is used to view line by line execution of program on grid.
- Debugging Information for All Robots is Shown in text area tagged "debug info here".
