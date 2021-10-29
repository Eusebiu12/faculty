(*LINK: 3. https://1drv.ms/t/s!Alm__d0jPPHD1_okEJKHOE0eogvtvw?e=gaGGUA*)
(*Problema 1*)
(*normal recursiv*)
let t i = 1. /. (4. *. float_of_int i *. (2. *. float_of_int i -. 1.))
let rec jumateLn2 n = if n=0 then 0.
else t(n) +. jumateLn2(n-1)
let r = jumateLn2 10000;;

(*Tail recursiv*)
let rec jumateLn3 n p = if n=0 then p
else jumateLn3 (n-1) (p +. t(n))
let r = jumateLn3 10000 0. ;;


(*Problema 2 a*)
let rec insert v lst lf = match lst with
| [] -> lf
| h::t -> if (h mod 2 =0) then insert v t (v::h::lf)
          else insert v t (h::lf)
let r = List.rev(insert 0 [1;2;3;4;5;6;7;8] []);;

(*Problema 2 b*)
let rec insert2 cond v lst lf = match lst with
| [] -> lf
| h::t -> if( cond h ) then insert2 cond v t (v::h::lf)
          else insert2 cond v t (h::lf)

let r = List.rev(insert2 (fun x -> x > 5) 0 [1;2;3;4;5;6;7;8] []);;


(*c. Returnează valoarea medie a cifrei de afaceri petnru magazinele cu un anumit domeniu(varinată simplă returnați suma și count)*)


let magazine = [
(*   Demunire, Cifra de afaceri (mil),      Domeniu *)
  (     "H&M",                  120.0,     "Fashion");
  ("Kaufkand",                  190.0, "Supermarket");
  (     "C&A",                   90.0,     "Fashion");
  ( "Carfour",                  190.0, "Supermarket");
  (   "Profi",                  290.0, "Supermarket");
  ( "Dedeman",                   90.0,    "Bricolaj");
]


(*let ra = magazineByDomeniu "Fashion" magazine
(* [("H&M", 120., "Fashion"); ("C&A", 90., "Fashion")] *)

let rb = numeMagazineByDomeniu "Fashion" magazine
(* ["H&M"; "C&A"] *)

let rc = avgCaMagazineByDomeniu "Fashion" magazine
(* 105. ) *)*)

(*Problema 3 a*)
let domeniu dom1 magazine = magazine
|> List.filter ( fun (dem,cif,dom2) -> dom1=dom2)
|> List.map (fun (dem,cif,dom2) -> dem,cif,dom2)
let r = domeniu "Fashion" magazine;;

(*Problema 3 b*)
let domeniu2 dom3 magazine = magazine
|> List.filter ( fun (dem,cif,dom2) -> dom3=dom2)
|> List.map (fun (dem,cif,dom2) -> dem)
let r = domeniu2 "Fashion" magazine;;

(*Problema 3 c*)
let medie_cifra dom magazine = magazine
|> List.filter ( fun (dem,cif,dom2) -> dom=dom2)
let count = medie_cifra "Fashion" magazine
let count2 cifra count = count
|> List.map (fun (dem,cif,dom2) -> cif-cif+cifra) (*Aici am vrut sa vad cate cifre de afaceri sunt: 6*)

let r = count2 1 count
let suma = medie_cifra "Fashion" magazine
let suma2 cif1 suma = suma                    (*Aici am vrut sa calculez suma*)
|> List.map ( fun (dem,cif,dom2) -> cif1+cif)
let r = suma2 0. suma 

let tupla = ( (suma2 0 suma),(count2 1 count) );;