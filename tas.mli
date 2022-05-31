type 'a heap ={mutable t:'a array;mutable n:int};;

val create_heap : int->'a->'a heap;;

val is_empty : 'a heap -> bool ;;

val add : 'a -> 'a heap -> unit;;

val extract : 'a heap -> 'a;;
