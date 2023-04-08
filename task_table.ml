



open List;;
type res = {name_studentstring; name_teacherstring; name_coursestring; resultint};;

let lst = [
  {name_student = Alexsander; name_teacher= Vladimir; name_course=CS; result = 98};
  {name_student = Nikolai; name_teacher= Vladimir; name_course=CS; result = 85};
  {name_student = Geoerge; name_teacher= Vladimir; name_course=CS; result = 97}; 
  {name_student = Johan; name_teacher= Vladimir; name_course=S; result = 90}; 
  {name_student = Johan; name_teacher= Vladimir; name_course=hS; result = 82}; 
  {name_student = Johan; name_teacher= Vladimir; name_course=lS; result = 83}; 
  {name_student = Johan; name_teacher= Genadiy; name_course=lllS; result = 90}; 
  {name_student = Alexsander; name_teacher= Genadiy; name_course=CS; result = 93};

];;
  
let rec find = function 
  []- 0
  ht- if (h.name_course  
              match t with
              []-h.name_course
              h2t-h2.name_course) then 1 + find t else find t ;; 


let f0 l = 1 + (find l);;

f0 lst;;

let rec find_teacher list name = 
  match list with
  []-
  ht - if (h.name_student = name) then h.name_teacher else find_teacher t name;;


let str name = find_teacher lst name;;


let rec find_num_courses list name = 
  match list with 
  []- 0
  ht - if (h.name_student = name && h.name_teacher = str name) then 1 + find_num_courses t name else find_num_courses t name;;
          

let rec find_res_courses list name = 
  match list with 
  []- 0
  ht - if (h.name_student = name && h.name_teacher = str name ) then h.result + find_res_courses t name else find_res_courses t name ;;




let mid list name =
  float_of_int(find_res_courses list name) . float_of_int(find_num_courses list name);;



let rec f1 list  =
  match list with 
  []-[]
  ht - if t  [] then (h.name_student, ( (find_teacher list h.name_student), (mid list h.name_student) ) )   f1 t  
      else [(h.name_student, ( (find_teacher list h.name_student), (mid list h.name_student) ) )];;


f1 lst 

(
f1 lst ;;



  let f2 list = 
    match list with 
    []-[]
    ht - f1 list 
               if 
  
  
  )
  
  
