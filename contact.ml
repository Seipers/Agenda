
module type CONTACT =
sig
  exception Add_Contact_With_Invalid_Data
  type contact
  val create : (string * string * int * string * string) -> contact
  val sort_contact : contact -> contact -> int
  val getFirstName : contact -> string
  val getLastName : contact -> string
  val getAge : contact -> int
  val getEmail : contact -> string
  val getPhone : contact -> string
  val find_firstname : contact list -> string -> contact list -> int
  val find_lastname : contact list -> string -> contact list -> int
  val find_age : contact list -> string -> contact list -> int
  val find_email : contact list -> string -> contact list -> int
  val find_phone : contact list -> string -> contact list -> int
  val global_search : contact list -> string -> int
  val print_all : contact -> int -> unit
  val print_firstname : string -> int -> unit
  val print_lastname : string -> int -> unit
  val print_age : string -> int -> unit
  val print_email : string -> int -> unit
  val print_phone : string -> int -> unit
  val print_id : string -> int -> unit
  val epur_str : string -> string
  val verif_age : int -> bool
  val verif_phone : string -> int -> bool
  val verif_email : string -> int -> bool
  val strstr : string -> string -> int -> int -> bool
end

module Contact : CONTACT =
struct
  exception Add_Contact_With_Invalid_Data
  type contact = (string * string * int * string * string)

  let rec epur_str = function
    |"" -> ""
    |str when str.[0] == ' ' -> ((String.sub str 0 1)^String.capitalize_ascii (epur_str(String.trim str)))
    |str -> ((String.sub str 0 1)^epur_str(String.sub str 1 ((String.length str) - 1)))

  let verif_age age =
    match age with
      | age when age > 120 -> false
      | age when age < 0 -> false
      | _ -> true

  let rec verif_phone phone c =
    match phone with
      | phone when String.length phone != 14 -> false
      | _ ->  match c with
          | c when c == 14 -> true
          | c when c mod 3 == 2 && (String.get phone c) != ' ' -> false
          | c when c == 0 && (String.get phone c) != '0' -> false
          | c when (c mod 3 == 0 || c mod 3 == 1) && ((String.get phone c) < '0' || (String.get phone c) > '9') -> false
          | _ -> verif_phone phone (c + 1)

  let verif_email email length =
    let rec verif_mail email length c at dot =
      match c with
	| c when c > length && at == true && dot == true -> true
	| c when c > length -> false
	| c when c <= length && at == false && email.[c] == '@' && c != 0-> verif_mail email length (c + 1) true dot
	| c when c <= length && dot == false && at == true && email.[c] == '.' && email.[c - 1] != '@' -> verif_mail email length (c + 1) at true
	| _ -> verif_mail email length (c + 1) at dot
    in verif_mail email length 0 false false

  let create (f, l, a, e, p) =
    if verif_age a == false then raise Add_Contact_With_Invalid_Data
    else if verif_email e (String.length e) == false then raise Add_Contact_With_Invalid_Data
    else if verif_phone p 0 == false then raise Add_Contact_With_Invalid_Data
    else (String.capitalize_ascii (String.trim (epur_str f)), String.uppercase_ascii (String.trim (epur_str l)), a, e, p)

  let getFirstName (f, _, _, _, _) = f
  let getLastName (_, l, _, _, _) = l
  let getAge (_, _, a, _, _) = a
  let getEmail (_, _, _, e, _) = e
  let getPhone (_, _, _, _, p) = p

  let sort_contact (f, l, _, _, _) (f2, l2, _, _, _) =
    if String.compare f f2 == 0
    then String.compare l l2
    else String.compare f f2

  let rec strstr  str1 str2 c c2 =
    match c with
      |c when c >= (String.length str1)  -> false
      |_ -> match str1 with
          |str1 when str1.[c] == str2.[c2] && c2 >= ((String.length str2) - 1) -> true
          |str1 when str1.[c] == str2.[c2] && c2 < ((String.length str2) - 1) -> strstr str1 str2 (c + 1) (c2 + 1)
          |_ -> strstr str1 str2 (c + 1) 0

  let rec find_firstname list fnd slist=
    match list with
      |[] -> ((List.length slist) * -1) + -1
      |_ -> match fnd with
	  |fnd when (strstr (String.lowercase_ascii (getFirstName (List.hd list)))
		       (String.lowercase_ascii fnd) 0 0) == true -> 0
	  |_ -> 1 + (find_firstname (List.tl list) fnd slist)

  let rec find_lastname list fnd slist=
    match list with
      |[] -> ((List.length slist) * -1) + -1
      |_ -> match fnd with
	  |fnd when (strstr (String.lowercase_ascii (getLastName (List.hd list)))
		       (String.lowercase_ascii fnd) 0 0) == true -> 0
	  |_ -> 1 + (find_lastname (List.tl list) fnd slist)

  let rec find_age list fnd slist=
    match list with
      |[] -> ((List.length slist) * -1) + -1
      |_ -> match fnd with
	  | fnd when (strstr (String.lowercase_ascii (string_of_int(getAge (List.hd list))))
			(String.lowercase_ascii fnd) 0 0) == true -> 0
	  | _ -> 1 + (find_age (List.tl list) fnd slist)

  let rec find_email list fnd slist=
    match list with
      |[] -> ((List.length slist) * -1) + -1
      |_ -> match fnd with
	  |fnd when (strstr (String.lowercase_ascii (getEmail (List.hd list)))
		       (String.lowercase_ascii fnd) 0 0) == true -> 0
	  |_ -> 1 + (find_email (List.tl list) fnd slist)

  let rec find_phone list fnd slist=
    match list with
      |[] -> ((List.length slist) * -1) + -1
      |_ -> match fnd with
	  |fnd when (strstr (String.lowercase_ascii (getPhone (List.hd list)))
		       (String.lowercase_ascii fnd) 0 0) == true -> 0
	  |_ -> 1 + (find_phone (List.tl list) fnd slist)

  let global_search  list fnd=
    match list with
      |[] -> -1
      |_ -> match list with
	  | list when find_firstname list fnd list != -1 -> find_firstname list fnd list
	  | list when find_lastname list fnd list != -1 -> find_lastname list fnd list
	  | list when find_age list fnd list != -1 -> find_age list fnd list
	  | list when find_email list fnd list != -1 -> find_email list fnd list
	  | list when find_phone list fnd list != -1 -> find_phone list fnd list
	  | _ -> -1

  let rec print_firstname firstname c =
    match c with
      | c when c > 15 -> ()
      | _ -> match firstname with
	  |"" -> print_char ' ' ; print_firstname firstname (c + 1)
	  | _ -> print_char (String.get firstname 0) ; print_firstname (String.sub firstname 1 ((String.length firstname) - 1)) (c + 1)

  let rec print_lastname lastname c =
    match c with
      | c when c > 15 -> ()
      | _ -> match lastname with
	  |"" -> print_char ' ' ; print_lastname lastname (c + 1)
	  | _ -> print_char (String.get lastname 0) ; print_lastname (String.sub lastname 1 ((String.length lastname) - 1)) (c + 1)

  let rec print_age age c =
    match c with
      | c when c > 3 -> ()
      | _ -> match age with
	  |"" -> print_char ' ' ; print_age age (c + 1)
	  | _ -> print_char (String.get age 0) ; print_age (String.sub age 1 ((String.length age) - 1)) (c + 1)

  let rec print_email email c =
    match c with
      | c when c > 31 -> ()
      | _ -> match email with
	  |"" -> print_char ' ' ; print_email email (c + 1)
	  | _ -> print_char (String.get email 0) ; print_email (String.sub email 1 ((String.length email) - 1)) (c + 1)

  let rec print_phone phone c =
    match c with
      | c when c > 13 -> ()
      | _ -> match phone with
	  |"" -> print_char ' ' ; print_phone phone (c + 1)
	  | _ -> print_char (String.get phone 0) ; print_phone (String.sub phone 1 ((String.length phone) - 1)) (c + 1)

  let rec print_id id c =
    match c with
      | c when c > 3 -> ()
      | _ -> match id with
	  |"" -> print_char ' ' ; print_id id (c + 1)
	  | _ -> print_char (String.get id 0) ; print_id (String.sub id 1 ((String.length id) - 1)) (c + 1)

  let print_all contact id =
    begin
      print_id (string_of_int id) 0;
      print_firstname (getFirstName contact) 0;
      print_lastname (getLastName contact) 0;
      print_age (string_of_int (getAge contact)) 0;
      print_email (getEmail contact) 0;
      print_phone (getPhone contact) 0;
    end
end
