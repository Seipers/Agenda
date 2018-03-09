include Agenda

let list = Agenda.addContact (Agenda.addContact (Agenda.addContact [] ("bozo   le   clown", "World", 1, "bozo-world@plop.fr", "06 36 45 89 67")) ("Seb", "Vidal", 100, "vidal_s@epitech.eu", "06 67 82 36 78")) ("Seb", "LG", 19, "le-gui_s@epitech.eu", "06 48 18 71 62")

let p = Agenda.printContacts list LastName ""; print_char '\n'

let nb = Agenda.getContactId list Age "100"
let p = print_char '\n'
let p = print_int nb
let p = print_char '\n'

let newlist = Agenda.removeContact list (Agenda.getContactId list Age "100")

let pr = Agenda.printContacts newlist Age ""

let nlist = Agenda.replaceContact newlist 1 ("   le nouveau", "nul", 5, "trucnul@nul.fr", "09 45 56 78 12")

let p = print_char '\n'

let pr = Agenda.printContacts nlist FirstName ""

let nb = Agenda.getContactId list LastName "vi"
let p = print_char '\n'
let p = print_int nb
let p = print_char '\n'
