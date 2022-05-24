open Tas

type graphe = {mat:int array array; sommet:char array;mutable n:int};;

let create_graphe(len:int):graphe=
  let g = {mat=Array.make len [||];sommet = Array.make len '~';n=0} in
  for i=0 to len-1 do
    g.mat.(i) <- Array.make len 0
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

let ajout_arc(a:char)(b:char)(w:int)(g:graphe):unit = g.mat.(trouve_ind a g).(trouve_ind b g) <- w;;

let init_dist(d:char)(g:graphe):int array=
  let dist = Array.make g.n max_int in
  dist.(trouve_ind d g) <- 0;
  dist
;;

let dijkstra(d:char)(a:char)(g:graphe):int=
  let dist = init_dist d g in
  let q = create_heap g.n in
  let u = ref (0,0) in
  let alt = ref 0 in
  for i=0 to g.n -1 do
    add((dist.(i),i) q);
  done;
  while not(is_empty q) do
    u := snd(extract q);
    for i=0 to g.n do
      if g.mat.(!u).(i) <> 0 then begin
          alt := dist.(!u) + g.mat.(!u).(i);
          if !alt < dist.(i) then begin
              dist.(i) <- !alt;
              add((!alt,i) q);
          end;
      end;
    done;
  done;
  dist.(trouve_ind a g)
;;
let g = create_graphe 4;;
ajout_sommet 'A' g;;
ajout_sommet 'B' g;;
ajout_sommet 'C' g;;
ajout_sommet 'D' g;;
ajout_arc 'A' 'B' 5 g;;
g
