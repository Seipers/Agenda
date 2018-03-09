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

module Contact : CONTACT
