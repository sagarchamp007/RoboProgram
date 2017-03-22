module Robo_parser where

import Conf_type exposing (..)
import Markdown
import String exposing (..)
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num exposing (..)
import Combine.Infix exposing (..)
import Result
import Regex exposing (..)
import Text
import Dict exposing (..)
import List.Extra exposing (unique,groupBy)



errors = Dict.fromList[(1,"function definition required"),(2,"Basic or Conditional statements required follwing a }"),(3,"newline required"),(4,"main function missing"),(5,"call to a absent function"),(6,"function name required"),(7,"Block Required"),(8,"No Input to parse")]



get_errors err_no error = [Maybe.withDefault "No error Defined" (get err_no errors)]

get_errors1 err_no = Maybe.withDefault "No error Defined" (get err_no errors)





-- conv_tree str = Root str [Empty]
last x = case x of
           [] -> ""
           [e] -> e
           (e::ls)-> last ls



conv_stmt stmt = case stmt of
                  "TurnLeft"-> TurnLeft
                  "TurnRight"->TurnRight
                  "GoForward"->GoForward
                  "PutCoin"->PutCoin
                  "PickCoin"->PickCoin
                  _ -> Call stmt


conv_condition condition = case condition of
                            "IsNextWall"-> IsNextWall
                            "IsRobotInNextCell"-> IsRobotInNextCell
                            "IsCoinInCurrentCell"->IsCoinInCurrentCell
                            "IsPocketEmpty"->IsPocketEmpty
                            _ -> IsError
spaces =  many1 (space `or` tab)
opt_space = many(space `or` tab)
new_lines = mapError (\_->["expected new line"]) <|Combine.map (\x-> last x) (many1 (opt_space *> (between (char '<') (char '>') (Combine.regex "[0-9]+")) <* opt_space))
new str = Result.withDefault 0 (String.toInt str)   

parse_block = let
                
                openBrac = (new_lines *> mapError (get_errors 7) (string "{"))
                stmt      =  let
                                 
                                 f_name = Combine.regex "[a-z]+"
                                 call =  (string "Call") *> spaces *> f_name
                                 parse_basic_cmd = (choice <| (++) (List.map (string) ["TurnLeft" ,"TurnRight" ,"GoForward" ,"PutCoin" ,"PickCoin"]) <| [call])
                                 parse_basic_stmt li stmt =  succeed <| BasicStmt (Line (new li)) (conv_stmt stmt)
                                                        

                              
                                 parse_cond_stmt li  = let
                                    parse_cond = choice <|List.map (string) ["IsNextWall","IsRobotInNextCell","IsCoinInCurrentCell","IsPocketEmpty","IsError"]
                               
                                
                                    parans = string "(" *> parse_cond <* string ")"
                                    else_block = opt_space *> string "else" *> rec(\()->parse_block)
                                    if_block = rec(\()->parse_block)
                                    fmt_cond cnd ifB line2 elB = ConditionalStmt (Line (new li)) (If (conv_condition cnd) ifB) (Line (new line2)) (Else elB)  
                                                       in  ((Combine.map fmt_cond parans) `andMap` if_block `andMap` new_lines `andMap` else_block)
                            
                                 decide li stmt = case stmt of
                                                 "if" -> parse_cond_stmt li
                                                 _ -> parse_basic_stmt li stmt
                                 stmt_start = (parse_basic_cmd `or`(string "if" <* opt_space))
                                 stmt_start_errors = mapError (get_errors 2) stmt_start

                             in  ((new_lines) `Combine.andThen` (\li-> stmt_start_errors `Combine.andThen` (decide li)))
                stmts = many1 (stmt) 
                c_brace = (new_lines *> mapError (get_errors 2) (string "}"))
              in Combine.map Block (openBrac *> stmts <* c_brace)         



function li = let
               func_name =  (mapError(get_errors 6)(Combine.regex"[a-z]+")) <* opt_space <* string "()"
               block = parse_block
               fn fn bl = [Function (Line (new li)) (FuncName fn) bl]
                     in (Combine.map fn func_name)  `andMap` (block)


parser_1 = let
              decide li = case li of
                                 ""->  succeed []
                                 _    -> Combine.map (++) (function li) `andMap` rec(\()->functions)
              func_start = (new_lines <* string "def" <* space) `or` ((succeed ""<* end) `or` (new_lines *>string "" <* end))
              func_start_err = mapError (get_errors 1) func_start 
              functions =  func_start_err `Combine.andThen` (decide)
      in Combine.map (Functions) (functions)



eval_stmts stmt  = case stmt of
                            [] -> []
                            ((BasicStmt (Line li) (Call name))::rls) -> [(name,li)] ++ eval_stmts rls
                            ((BasicStmt li (cmd))::rls) -> (eval_stmts rls)
                            ((ConditionalStmt li (If cnd (Block bls)) li1 (Else (Block bls1)))::rls) -> (eval_stmts bls) ++ (eval_stmts bls1) ++ (eval_stmts rls)
                            

get_calls ls = case ls of
                        [] -> []
                        ((Function (a) (b) (Block bls)) :: rls) -> (eval_stmts bls) ++ (get_calls rls) 
                        
                        
                     
                     

get_names ls = case ls of
                   []->[]
                   ((Function (Line li) (FuncName name) (block)) :: rls) -> (name,li) :: (get_names rls)

validate_fun func_ls = let
                        dup_ls = groupBy (\x y-> fst x == fst y) <|List.sortBy (\(x,y)->x) func_ls
                        check_dup_ls lst = case lst of
                                            [] -> ""
                                            (e::ls) -> if ((List.length e)>1)   then ("Multiple definitions of " ++ ( fst <|Maybe.withDefault ("",0) (List.head e)) ++ "() in lines "  ++   (toString <| (List.map (\(x,y)-> y) e)) ++ "\n" ++ check_dup_ls ls) else (check_dup_ls ls)

                        get_str = check_dup_ls dup_ls
                       in if( get_str == "") then Ok "" else Err get_str     



validate_calls call_lst func_lst = let
                                      func_names_ls = List.map (\(x,y)->x) func_lst
                                      get_member (x,y) =  if(List.member x func_names_ls) then "" else (toString y)
                                      str_ls1 = List.map  get_member call_lst
                                      str_ls = List.filter ((/=) "") str_ls1 
                                      allTrue = List.all (\x->x=="")   <| str_ls

                                   in if(str_ls==[]) then (Ok "") else  (Err ("invalid call in lines " ++ toString str_ls ++"\n") )

parser_2 (Functions ls) = let
                              calls_ls = get_calls ls
                              func_names_ls = get_names ls
                              check_funNames = validate_fun func_names_ls
                              check_main ls =  List.length (List.filter (\(x,y) -> x=="main") ls) /= 0
                          in case check_funNames of
                                                    Ok value ->  case (validate_calls calls_ls func_names_ls) of
                                                                   Ok value -> if (check_main func_names_ls) then (Ok (Functions ls)) else Err "No Main Declared \n"
                                                                   Err err ->  Err err
                                                    Err err -> Err err


last_l lst = case lst of
              []-> 0
              [e]->e
              (e::ls) -> last_l ls 

deter_pos pos tuple = case tuple of
                   ([],[]) ->(0,0)
                   (ls,[])-> (last_l ls,pos)
                   (a,b)     -> (last_l a,Maybe.withDefault 0 <|List.head b)

format_error error context prog =
                            let corr_error str pos = deter_pos pos <|List.partition (\x->x<pos) <|List.map .index <|Regex.find All (Regex.regex "<[0-9]+>") prog 
                                positions = (corr_error context.input context.position)
                            in  "Line" ++ (slice (fst(positions)) (context.position) prog ) ++ "??" ++ (slice (context.position) (snd(positions)) prog) ++ "\nError -  " ++ (String.join " or " error)  ++ "\n" 


extract (result,context) prog = case result of
                                  Ok value -> parser_2 value
                                  Err error -> Err (format_error error context prog) 






parser prog = extract (parse parser_1 prog) prog



addinfo i string= case string of
                   []-> ""
                   (e :: ls) -> if(e =='\n') then ("<"++ (toString i) ++ ">  " ++ (addinfo (i+1) ls)) else ((e `String.cons` "") ++ (addinfo i ls))  



parse_it prog = let
                 add_prog = ((++) "<1> ") <| prog
                 infoAdd = (addinfo 2) <| String.toList (add_prog)
                 parser_result = parser infoAdd
                in case prog of
                    "" -> Err ""
                    _  -> parser_result


