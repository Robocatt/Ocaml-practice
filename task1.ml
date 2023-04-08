(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button. *)

type nsp = string;;
type course = string;;
type grade = int;;
type info = Info of nsp * nsp * course * grade;; 
open List;;
open Float;;
let info = [ 
  Info ("Student 1", "Teach 1","Corse 10", 5);
  Info ("Student 1", "Teach 1","Corse 1", 5);
  Info ("Student 2", "Teach 1","Corse 2", 3);
  Info ("Student 3", "Teach 1","Corse 3", 2);
  Info ("Student 1", "Teach 2","Corse 1", 4);
  Info ("Student 4", "Teach 3","Corse 4", 5);] ;;
let getCourse inf =
  match inf with 
    Info(_,_,course,_) -> course ;; 

let rec f data = 
  fold_right (fun x y -> 
      if not ( mem (getCourse x) y) then (getCourse x) :: y else  y) data [] ;; 

let rec f0 data = 
  match f info with 
  |h::t-> fold_left (fun acc _ -> acc + 1) 0 t + 1 
  |[]->0;;
f0 info;; 


let getTF data = 
  match data with
    Info(_,fio,_,_)->fio;;

let rec ft data = 
  fold_right (fun x y -> 
      if( not ( mem (getTF x) y ) )then (getTF x)::y 
      else y) data [];;

let getSF data = 
  match data with 
    Info(fio,_,_,_) -> fio;;

let rec fs data  = 
  fold_right (fun x y ->
      if not (mem (getSF x) y ) then (getSF x) :: y
      else y) data [];;
  
let rec getStudentList student data = fold_right (fun x y ->  
    if match x with 
        Info (fio,_,_,_)-> fio = student 
    then x :: y 
    else y) data [];;

let mid student data = fold_right (fun teacher arr -> 
    fold_right (fun subject (name,count,summ) ->
        if match subject with 
            Info(_,teach,_,_) -> teach = teacher 
        then match subject with Info(_,_,_,grade) -> (teacher, count+1,summ + grade)
        else (name,count,summ))
      (getStudentList student data) (teacher,0,0)::arr) (ft data) [];;

let makepair data inf res = (inf, mid inf data) :: res;; 

let rec getstat data = fold_right (makepair data) (fs data) [] ;;


let f1 data = map(fun (name,array) ->
    (name, sort (fun (_,f) (_,s)->
         if f < s then 1 
         else if f = s then 0
         else -1)
        (filter (fun (_,x) ->
             if is_nan x then false else true)
            (map (fun (x,y,z) -> (x,of_int (z) /. of_int (y) ) ) array)) )) 
    (getstat data);;


f1 info;;

let f2 data = fold_right (fun (name, (teach,grade) :: arr ) y -> (name,teach)::y) (f1 data ) [];; 
f2 info;;
  
  
  
  
  









  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
