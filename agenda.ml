exception Remove_Impossible_On_An_Empty_List
exception Remove_Using_An_Invalid_Id
exception Replace_Impossible_On_An_Empty_List
exception Replace_Using_An_Invalid_Id
exception Replace_Contact_With_Invalid_Data
exception Add_Contact_With_Invalid_Data


open Contact
type field = All | Id | FirstName | LastName | Age | Email | Phone
module type AGENDA =
sig
  val addContact     : Contact.contact list -> string * string * int * string * string -> Contact.contact list
  val getContactId   : Contact.contact list -> field -> string -> int
  val removeContact  : Contact.contact list -> int -> Contact.contact list
  val replaceContact : Contact.contact list -> int -> string * string * int * string * string -> Contact.contact list
  val printContacts  : Contact.contact list -> field -> string -> unit
end

module Agenda : AGENDA =
struct

  let addContact list c =
  try
    List.sort Contact.sort_contact (List.append list [(Contact.create c)])
  with
  | Contact.Add_Contact_With_Invalid_Data -> raise Add_Contact_With_Invalid_Data

  let getContactId list f info =
    match f with
      | Id -> (int_of_string info)
      | FirstName -> (Contact.find_firstname list info list)
      | LastName -> (Contact.find_lastname list info list)
      | Age -> (Contact.find_age list info list)
      | Email -> (Contact.find_email list info list)
      | Phone -> (Contact.find_phone list info list)
      | All -> (Contact.global_search list info)

  let removeContact list id =
    match list with
      |[] -> raise Replace_Impossible_On_An_Empty_List
      |_ -> match id with
	  |id when id >= List.length list -> raise Remove_Using_An_Invalid_Id
	  |id when id < 0 -> raise Remove_Using_An_Invalid_Id
	  |_ -> let continue newlist contact =
		  match list with
		    |_ -> List.filter (fun x -> (x = contact) = false) list
		in continue list (List.nth list id)

    let replaceContact list rem c =
    try
      addContact (removeContact list rem) c
    with
    | Replace_Impossible_On_An_Empty_List -> raise Replace_Impossible_On_An_Empty_List
    | Remove_Using_An_Invalid_Id -> raise Replace_Using_An_Invalid_Id
    | Add_Contact_With_Invalid_Data -> raise Replace_Contact_With_Invalid_Data

  let print_one contact f c =
    match c with
      |c when c < 0 -> ()
      |_ -> Contact.print_all contact c

  (* |_ -> match f with *)
  (* 	  | All -> Contact.print_all contact c *)
  (* 	  | Id -> Contact.print_id (string_of_int c) 0 *)
  (* 	  | FirstName -> Contact.print_firstname (Contact.getFirstName contact) 0 *)
  (* 	  | LastName -> Contact.print_lastname (Contact.getLastName contact) 0 *)
  (* 	  | Age -> Contact.print_age (string_of_int (Contact.getAge contact)) 0 *)
  (* 	  | Email -> Contact.print_email (Contact.getEmail contact) 0 *)
  (* 	  | Phone -> Contact.print_phone (Contact.getPhone contact) 0 *)

  let rec print_all_list list f c =
    match list with
      |[] -> ()
      |_ ->  print_one (List.hd list) f c ; print_char '\n' ; print_all_list (List.tl list) f (c + 1)

  let printContacts list f info =
    match info with
      |"" -> print_all_list list f 0
      |_ -> print_one (List.nth list (getContactId list f info)) f (getContactId list f info)
end
