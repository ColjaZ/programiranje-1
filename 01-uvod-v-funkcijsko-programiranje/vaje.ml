(*----------------------------------------------------------------------------*
 # Uvod v funkcijsko programiranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Vektorji
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razteg : float -> float list -> float list`, ki vektor,
 predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let razteg1 koeficient vektor =
  List.map (fun x -> x *. koeficient) vektor 

let razteg2 koeficient vektor =
  let f_razteg x = koeficient *. x in
  List.map f_razteg vektor

let razteg3 koeficient vektor =
  List.map (( *. ) koeficient) vektor (*ekvivalentne definicije te funkcije. ta tzadnja ne vem ce je prov*)

let primer_vektorji_1 = razteg1 2.0 [1.0; 2.0; 3.0]
(* val primer_vektorji_1 : float list = [2.; 4.; 6.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sestej : float list -> float list -> float list`, ki vrne
 vsoto dveh vektorjev.
[*----------------------------------------------------------------------------*)

let sestej1 v1 v2 =
  List.map2 (+.) v1 v2

let sestej2 =
  List.map2 (+.) (* ta vrne funkcije ki sprejme se dva argumenta (v nasem primeru bi vstavili v1 in v2)*)

let sestej3 v1 v2 =
  let pari = List.combine v1 v2 in
  List.map (fun (c1, c2) -> c1 +. c2) pari

let primer_vektorji_2 = sestej1 [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_2 : float list = [5.; 7.; 9.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `skalarni_produkt : float list -> float list -> float`, ki
 izračuna skalarni produkt dveh vektorjev. Pri tem si lahko pomagate s funkcijo
 `vsota_seznama : float list -> float`, definirano prek funkcije
 `List.fold_left`, ki jo bomo spoznali kasneje:
[*----------------------------------------------------------------------------*)

let vsota_seznama = List.fold_left (+.) 0.

let skalarni_produkt v1 v2 =
  let v3 = List.map2 ( *. ) v1 v2 in
  vsota_seznama v3

let primer_vektorji_3 = skalarni_produkt [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_3 : float = 32. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `norma : float list -> float`, ki vrne evklidsko normo
 vektorja.
[*----------------------------------------------------------------------------*)

let norma v1 =
  let v3 = List.map2 ( *. ) v1 v1 in
  sqrt (vsota_seznama v3)

let primer_vektorji_4 = norma [3.0; 4.0]
(* val primer_vektorji_4 : float = 5. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vmesni_kot : float list -> float list -> float`, ki izračuna
 kot med dvema vektorjema v radianih.
[*----------------------------------------------------------------------------*)

let vmesni_kot v1 v2 =
  acos ((skalarni_produkt v1 v2) /. (norma v1 *. norma v2))

let primer_vektorji_5 = vmesni_kot [1.0; 0.0] [0.0; 1.0]
(* val primer_vektorji_5 : float = 1.57079632679489656 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `normirani : float list -> float list`, ki normira dani
 vektor.
[*----------------------------------------------------------------------------*)

let normirani v1 =
  List.map (fun x -> x /. norma v1) v1

let primer_vektorji_6 = normirani [3.0; 4.0]
(* val primer_vektorji_6 : float list = [0.600000000000000089; 0.8] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `projeciraj : float list -> float list -> float list`, ki
 izračuna projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projekcija v1 v2 =
  List.map (fun x -> skalarni_produkt v1 v2 /. norma v2 *. x) v2

let primer_vektorji_7 = projekcija [3.0; 4.0] [1.0; 0.0]
(* val primer_vektorji_7 : float list = [3.; 0.] *)

(*----------------------------------------------------------------------------*
 ## Generiranje HTML-ja
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML
 oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.
[*----------------------------------------------------------------------------*)

let ovij oznaka vsebina =
  let o1 = "<" ^ oznaka ^ ">" in
  let o2 = "</" ^ oznaka ^ ">" in
  o1 ^ vsebina ^ o2

let primer_html_1 = ovij "h1" "Hello, world!"
(* val primer_html_1 : string = "<h1>Hello, world!</h1>" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število
 presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za
 ustrezno število presledkov.
[*----------------------------------------------------------------------------*)

let zamakni stevilo niz =
  let presledki = String.make stevilo ' ' in
  let seznam = String.split_on_char '\n' niz in
  let nov_seznam = List.mapi (fun i x -> (if i != List.length seznam - 1 then presledki ^ x ^ "\n" else presledki ^ x)) seznam in
  String.concat "" nov_seznam



let primer_html_2 = zamakni 4 "Hello,\nworld!"
(* val primer_html_2 : string = "    Hello,\n    world!" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne
 niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:
[*----------------------------------------------------------------------------*)
let ul nizi =
  let seznam = List.map (fun x -> ovij "li" x ^ "\n") nizi in
  let niz = ovij "ul" ("\n" ^ zamakni 2 (String.concat "" seznam)) in
  niz

let primer_html_3 = ul ["ananas"; "banana"; "čokolada"]
(* val primer_html_3 : string =
  "<ul>\n  <li>ananas</li>\n  <li>banana</li>\n  <li>čokolada</li>\n</ul>" *)

(*----------------------------------------------------------------------------*
 ## Nakupovalni seznam
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme
 niz, ki vsebuje vejico, loči na del pred in del za njo.
[*----------------------------------------------------------------------------*)

let razdeli_vrstico niz =
  let seznam = String.split_on_char ',' niz in
  (String.trim (List.nth seznam 0), String.trim (List.nth seznam 1))

let primer_seznam_1 = razdeli_vrstico "mleko, 2"
(* val primer_seznam_1 : string * string = ("mleko", "2") *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`,
 ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike `"izdelek,
 vrednost"`, in vrne seznam ustreznih parov.
[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz =
  let seznam = String.split_on_char '\n' niz in
  List.map (fun x -> razdeli_vrstico x) seznam

let primer_seznam_2 = pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"
(* val primer_seznam_2 : (string * string) list =
  [("mleko", "2"); ("kruh", "1"); ("jabolko", "5")] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list
 -> (string * 'b) list`, ki dano funkcijo uporabi na vseh drugih komponentah
 elementov seznama.
[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente funkcija seznam =
  List.map (fun (x,y) -> (x, funkcija y)) seznam

(* val primer_seznam_3 : (string * int) list = [("ata", 4); ("teta", 5)] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki
 sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni
 znesek nakupa.
[*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek cenik seznam =
  let izdelki = pretvori_v_seznam_parov seznam in
  let cene = pretvori_v_seznam_parov cenik in
  let izdelki_float = pretvori_druge_komponente (fun x -> float_of_string x) izdelki in
  let cene_float = pretvori_druge_komponente (fun x -> float_of_string x) cene in

  let cene = List.map (fun (x,y) -> y *. List.assoc x cene_float) izdelki_float in
  vsota_seznama cene

let primer_seznam_4 = 
  let nakupovalni_seznam = "mleko, 2\njabolka, 5"
  and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
  izracunaj_skupni_znesek cenik nakupovalni_seznam
(* val primer_seznam_4 : float = 5.5 *)
