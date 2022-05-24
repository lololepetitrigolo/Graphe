type 'a heap ={t:'a array;mutable n:int};;

let create_heap(len:int)(x:'a):'a heap={t=Array.make len x;n=0};;

let is_empty(h:'a heap):bool = h.n = 0;;

let swap(i:int)(j:int)(h:'a heap):unit=
  let mem = h.t.(j) in
  h.t.(j) <- h.t.(i);
  h.t.(i) <- mem;
;;

let rec sift_up(i:int)(h:'a heap):unit=
  if(i<> 0 && h.t.(i)>h.t.((i-1)/2)) then begin
      swap i ((i-1)/2) h;
      sift_up ((i-1)/2) h;
    end;
;;

let rec sift_down(i:int)(h:'a heap):unit=
  let j = ref i in
  if(2*i+1 < h.n && h.t.(i) < h.t.(2*i+1)) then j := 2*i+1;
  if(2*i+2 < h.n && h.t.(2*i+2) > h.t.(!j)) then j:= 2*i+2;
  if !j<>i then begin
      swap i !j h;
      sift_down !j h;
   end;
;;


let add(x:'a)(h:'a heap):unit=
  if h.n >= Array.length h.t then failwith "Plus de place dommage";
  h.t.(h.n) <- x;
  h.n <- h.n + 1;
  sift_up (h.n-1) h;
;;

let extract(h:'a heap):'a =
  if h.n = 0 then failwith "Tas vide on ne peut pas extraire";
  let res = h.t.(0) in
  h.t.(0) <- h.t.(h.n - 1);
  h.n <- h.n -1;
  sift_down 0 h;
  res
;;
