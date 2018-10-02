type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }

let wellformed date =
  date.year >= 1 
  && date.month >= 1 && date.month <=5 
  && date.day >= 1 && date.day <= 4 
  && date.hour >= 0 && date.hour <= 2
  && date.minute >=0 && date.minute <= 1 ;;

let next date =
  let new_date_min = { year = date.year; month = date.month; day = date.day;
                       hour = date.hour; minute = date.minute + 1 }
  and
    new_date_hour = { year = date.year; month = date.month; day = date.day;
                      hour = date.hour + 1; minute = 0 }
  and 
    new_date_day = { year = date.year; month = date.month; day = date.day + 1;
                     hour = 0; minute = 0 }
  and 
    new_date_month = { year = date.year; month = date.month + 1; day = 1;
                       hour = 0; minute = 0 }
  and 
    new_date_year = { year = date.year + 1; month = 1; day = 1;
                      hour = 0; minute = 0 }
  in
  if wellformed new_date_min then new_date_min else
    (if wellformed new_date_hour then new_date_hour else
       (if wellformed new_date_day then new_date_day else
          (if wellformed new_date_month then new_date_month else
             new_date_year))) ;;

let rec of_int minutes =
  match minutes with
  |0 -> the_origin_of_time
  |_ -> next (of_int (minutes - 1)) ;;

