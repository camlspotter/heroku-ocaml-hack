type 'a t
val create : thresh:int -> limit:int -> 'a t
val add : 'a t -> int -> 'a -> 'a t
val to_list : 'a t -> (int * 'a list) list
val thresh : 'a t -> int
