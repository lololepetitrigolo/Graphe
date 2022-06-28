open Tas

type graphe = {mat:float array array; sommet:char array;mutable n:int};;

let create_graphe(len:int):graphe=
  let g = {mat=Array.make len [||];sommet = Array.make len '~';n=0} in
  for i=0 to len-1 do
    g.mat.(i) <- Array.make len 0.
  done;
  g
;;

let ajout_sommet(a:char)(g:graphe):unit=
  g.sommet.(g.n)<-a;
  g.n <- g.n+1
;;

let trouve_ind(a:char)(g:graphe):int =
  let i = ref 0 in
  while !i < g.n && g.sommet.(!i) <> a do
    i := !i+1;
  done;
  !i
;;

let ajout_arc(a:char)(b:char)(w:float)(g:graphe):unit = g.mat.(trouve_ind a g).(trouve_ind b g) <- w;;

let init_dist(d:char)(g:graphe):float array=
  let dist = Array.make g.n infinity in
  dist.(trouve_ind d g) <- 0.;
  dist
;;

let dijkstra(d:char)(a:char)(g:graphe):float*(char list)=
  let pred = Array.make g.n (-1) in
  let dist = init_dist d g in
  let q = create_heap g.n (0.,0) in
  let u = ref 0 in
  let alt = ref 0. in
  for i=0 to g.n -1 do
    add (dist.(i),i) q;
  done;
  while not(is_empty q) do
    u := snd(extract q);
    for i=0 to g.n -1 do
      if g.mat.(!u).(i) <> 0. then begin
          alt := dist.(!u) +. g.mat.(!u).(i);
          if !alt < dist.(i) then begin
              dist.(i) <- !alt;
              add (!alt,i) q;
              pred.(i) <- !u
          end;
      end;
    done;
  done;
  let chemin = ref [a] in
  let s = ref pred.(trouve_ind a g)in
  let indd = trouve_ind d g in
  while !s <> -1 || !s = indd do
    chemin := g.sommet.(!s)::(!chemin);
    s := pred.(!s);
  done;
  if dist.(trouve_ind a g) <> infinity then (dist.(trouve_ind a g),!chemin)
  else (dist.(trouve_ind a g),[])
;;
(*
let g = create_graphe 8;;
ajout_sommet 'A' g;;
ajout_sommet 'B' g;;
ajout_sommet 'C' g;;
ajout_sommet 'D' g;;
ajout_sommet 'E' g;;
ajout_sommet 'F' g;;
ajout_sommet 'G' g;;
ajout_sommet 'H' g;;
ajout_arc 'A' 'B' 5. g;;
ajout_arc 'A' 'E' 10. g;;
ajout_arc 'A' 'H' 6. g;;
ajout_arc 'A' 'G' 7. g;;
ajout_arc 'B' 'F' 8. g;;
ajout_arc 'D' 'D' 1. g;;
ajout_arc 'D' 'E' 3. g;;
ajout_arc 'F' 'D' 7. g;;
ajout_arc 'G' 'F' 2. g;;
ajout_arc 'G' 'H' 2. g;;
ajout_arc 'H' 'A' 5. g;;
 *)
let g = create_graphe 13;;
ajout_sommet 'C' g;;
ajout_sommet 'a' g;;
ajout_sommet ' ' g;;
ajout_sommet 'm' g;;
ajout_sommet 'A' g;;
ajout_sommet 'r' g;;
ajout_sommet 'c' g;;
ajout_sommet 'h' g;;
ajout_sommet 'e' g;;
ajout_sommet '_' g;;
ajout_sommet 'p' g;;
ajout_sommet '@' g;;
ajout_sommet 's' g;;
ajout_arc 'C' 'a' 1. g;;
ajout_arc 'a' ' ' 1. g;;
ajout_arc ' ' 'm' 1. g;;
ajout_arc 'm' 'A' 4. g;;
ajout_arc 'A' 'r' 7. g;;
ajout_arc 'r' 'c' 8. g;;
ajout_arc 'c' 'h' 1. g;;
ajout_arc 'h' 'e' 3. g;;
ajout_arc 'e' '_' 7. g;;
ajout_arc '_' 'p' 2. g;;
ajout_arc 'p' '@' 2. g;;
ajout_arc '@' 's' 5. g;;

let res =  dijkstra 'C' 's' g in
    print_float (fst(res));
    print_string " ";
    List.map (fun x ->print_char x;) (snd(res));;
print_newline();;
g
