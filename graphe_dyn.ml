type dyn_graphe = {mat: float array array array;sommet:char array;mutable n:int};;

let create_graphe(len:int)(t:int):dyn_graphe=
  let g = {mat=Array.make len [||];sommet = Array.make len '~';n=0} in
  for i=0 to len-1 do
    let tab = Array.make len [||] in
    for j=0 to len-1 do
      let l = Array.make t 0. in
      tab.(j) <- l
    done;
    g.mat.(i) <- tab;
  done;
  g
;;


let ajout_sommet(a:char)(g:dyn_graphe):unit=
  g.sommet.(g.n)<-a;
  g.n <- g.n+1
;;

let trouve_ind(a:char)(g:dyn_graphe):int =
  let i = ref 0 in
  while !i < g.n && g.sommet.(!i) <> a do
    i := !i+1;
  done;
  !i
;;

let ajout_arc(a:char)(b:char)(w:float array)(g:dyn_graphe):unit =
  if Array.length w <> Array.length g.mat.(0).(0) then failwith "Pas la bonne longueur de temps";
  g.mat.(trouve_ind a g).(trouve_ind b g) <- w;;

let g = create_graphe 3 4;;
ajout_sommet 'A' g;;
ajout_sommet 'B' g;;
ajout_sommet 'C' g;;
ajout_arc 'A' 'B' [|5.;3.;6.;8.|] g;;
g
