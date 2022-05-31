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
  let dist = Array.make g.n (max_int/2) in
  dist.(trouve_ind d g) <- 0;
  dist
;;

let dijkstra(d:char)(a:char)(g:graphe):int=
  let dist = init_dist d g in
  let q = create_heap g.n (0,0) in
  let u = ref 0 in
  let alt = ref 0 in
  for i=0 to g.n -1 do
    add (dist.(i),i) q;
  done;
  while not(is_empty q) do
    u := snd(extract q);
    for i=0 to g.n -1 do
      if g.mat.(!u).(i) <> 0 then begin
          alt := dist.(!u) + g.mat.(!u).(i);
          if !alt < dist.(i) then begin
              dist.(i) <- !alt;
              add (!alt,i) q;
          end;
      end;
    done;
  done;
  dist.(trouve_ind a g)
;;

let g = create_graphe 8;;
ajout_sommet 'A' g;;
ajout_sommet 'B' g;;
ajout_sommet 'C' g;;
ajout_sommet 'D' g;;
ajout_sommet 'E' g;;
ajout_sommet 'F' g;;
ajout_sommet 'G' g;;
ajout_sommet 'H' g;;
ajout_arc 'A' 'B' 5 g;;
ajout_arc 'A' 'E' 10 g;;
ajout_arc 'A' 'H' 6 g;;
ajout_arc 'A' 'G' 7 g;;
ajout_arc 'B' 'F' 8 g;;
ajout_arc 'D' 'D' 1 g;;
ajout_arc 'D' 'E' 3 g;;
ajout_arc 'F' 'D' 7 g;;
ajout_arc 'G' 'F' 2 g;;
ajout_arc 'G' 'H' 2 g;;
ajout_arc 'H' 'A' 5 g;;

print_int (dijkstra 'H' 'F' g);;
print_newline();;
g
