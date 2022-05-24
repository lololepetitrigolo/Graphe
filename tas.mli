type 'a heap ={t:'a array;mutable n:int};;

val create_heap : int->'a->'a heap;;

val is_empty : 'a heap -> bool ;;

val add : 'a -> 'a heap;;

val extract : 'a heap -> 'a;;
