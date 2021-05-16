let default x = function None -> x | Some x -> x
let echo x = Printf.ksprintf print_endline x
let sf = Printf.sprintf
let int = int_of_float

let round f =
  if f >= 0. then
    truncate (f +. 0.5)
  else
    truncate (f -. 0.5)

let (%) a b =
  let m = a mod b in
  if m < 0 then m + b else m

let (%.) a b =
  let m = mod_float a b in
  if m < 0. then m +. b else m

let with_open_out filename f =
  let ch = open_out filename in
  Fun.protect ~finally: (fun () -> close_out ch) (fun () -> f ch)

let with_open_in filename f =
  let ch = open_in filename in
  Fun.protect ~finally: (fun () -> close_in ch) (fun () -> f ch)

let (//) = Filename.concat

let read_file ?(buffer_size = 1024) filename =
  with_open_in filename @@ fun ch ->
  let buffer = Buffer.create buffer_size in
  let bytes = Bytes.create buffer_size in
  let rec loop () =
    let len = input ch bytes 0 buffer_size in
    if len > 0 then (
      Buffer.add_subbytes buffer bytes 0 len;
      loop ()
    )
  in
  loop ();
  Buffer.contents buffer

module Option =
struct
  include Stdlib.Option

  let map' o f = map f o
  let fold' none o some = fold ~none ~some o
  let iter' o f = iter f o
  let equal' o1 o2 f = equal f o1 o2
  let compare' o1 o2 f = compare f o1 o2
end

module List =
struct
  include Stdlib.List

  let range a b =
    let rec loop acc b =
      if b < a then
        acc
      else
        loop (b :: acc) (b - 1)
    in
    if a > b then [] else loop [] b

  let iter' l f = iter f l
  let iteri' l f = iteri f l
  let map' l f = map f l
  let mapi' l f = mapi f l
  let rev_map' l f = rev_map f l
  let filter_map' l f = filter_map f l
  let concat_map' l f = concat_map f l
  let fold_left' acc l f = fold_left f acc l
  let fold_right' l acc f = fold_right f l acc
  let iter2' l1 l2 f = iter2 f l1 l2
  let map2' l1 l2 f = map2 f l1 l2
  let rev_map2' l1 l2 f = rev_map2 f l1 l2
  let fold_left2' acc l1 l2 f = fold_left2 f acc l1 l2
  let fold_right2' acc l1 l2 f = fold_right2 f l1 l2 acc
  let for_all' l f = for_all f l
  let exists' l f = exists f l
  let for_all2' l1 l2 f = for_all2 f l1 l2
  let exists2' l1 l2 f = exists2 f l1 l2
  let find' l f = find f l
  let find_opt' l f = find_opt f l
  let find_map' l f = find_map f l
  let filter' l f = filter f l
  let find_all' l f = find_all f l
  let partition' l f = partition f l
  let sort' l f = sort f l
  let stable_sort' l f = stable_sort f l
  let fast_sort' l f = fast_sort f l
  let sort_uniq' l f = sort_uniq f l
  let merge' l1 l2 f = merge f l1 l2
end

module Array =
struct
  include Stdlib.Array

  let iter' a f = iter f a
  let iteri' a f = iteri f a
  let map' a f = map f a
  let mapi' a f = mapi f a
  let fold_left' acc a f = fold_left f acc a
  let fold_right' a acc f = fold_right f a acc
  let iter2' a1 a2 f = iter2 f a1 a2
  let map2' a1 a2 f = map2 f a1 a2
  let for_all' a f = for_all f a
  let exists' a f = exists f a
  let sort' a f = sort f a
  let stable_sort' a f = stable_sort f a
  let fast_sort' a f = fast_sort f a
end

module Set =
struct
  include Stdlib.Set

  module type S =
  sig
    include Stdlib.Set.S

    val add_list: elt list -> t -> t

    val iter': t -> (elt -> unit) -> unit
    val map': t -> (elt -> elt) -> t
    val fold': t -> 'a -> (elt -> 'a -> 'a) -> 'a
    val for_all': t -> (elt -> bool) -> bool
    val exists': t -> (elt -> bool) -> bool
    val filter': t -> (elt -> bool) -> t
    val partition': t -> (elt -> bool) -> t * t
    val find_first': t -> (elt -> bool) -> elt
    val find_first_opt': t -> (elt -> bool) -> elt option
    val find_last': t -> (elt -> bool) -> elt
    val find_last_opt': t -> (elt -> bool) -> elt option

    val show: (elt -> string) -> t -> string
    val show': t -> (elt -> string) -> string
  end

  module Make (X: OrderedType) =
  struct
    include Make (X)

    let add_list l s = List.fold_left (fun acc x -> add x acc) s l

    let iter' s f = iter f s
    let map' s f = map f s
    let fold' s acc f = fold f s acc
    let for_all' s f = for_all f s
    let exists' s f = exists f s
    let filter' s f = filter f s
    let partition' s f = partition f s
    let find_first' s f = find_first f s
    let find_first_opt' s f = find_first_opt f s
    let find_last' s f = find_last f s
    let find_last_opt' s f = find_last_opt f s

    let show show_element set =
      match elements set with
        | [] -> "{}"
        | _ :: _ as l -> "{ " ^ String.concat "; " (List.map show_element l) ^ " }"

    let show' s f = show f s
  end

  module Char = Make (Char)
  module Uchar = Make (Uchar)
  module Int = Make (Int)
  module Int32 = Make (Int32)
  module Int64 = Make (Int64)
  module Float = Make (Float)
  module String = Make (String)
end

module Map =
struct
  include Stdlib.Map

  module type S =
  sig
    include Stdlib.Map.S

    val of_list: (key * 'a) list -> 'a t
    val add_list: (key * 'a) list -> 'a t -> 'a t

    val update': key -> 'a t -> ('a option -> 'a option) -> 'a t
    val merge': 'a t -> 'b t -> (key -> 'a option -> 'b option -> 'c option) -> 'c t
    val union': 'a t -> 'a t -> (key -> 'a -> 'a -> 'a option) -> 'a t
    val compare': 'a t -> 'a t -> ('a -> 'a -> int) -> int
    val equal': 'a t -> 'a t -> ('a -> 'a -> bool) -> bool
    val iter': 'a t -> (key -> 'a -> unit) -> unit
    val fold': 'a t -> 'b -> (key -> 'a -> 'b -> 'b) -> 'b
    val for_all': 'a t -> (key -> 'a -> bool) -> bool
    val exists': 'a t -> (key -> 'a -> bool) -> bool
    val filter': 'a t -> (key -> 'a -> bool) -> 'a t
    val partition': 'a t -> (key -> 'a -> bool) -> 'a t * 'a t
    val find_first': 'a t -> (key -> bool) -> key * 'a
    val find_first_opt': 'a t -> (key -> bool) -> (key * 'a) option
    val find_last': 'a t -> (key -> bool) -> key * 'a
    val find_last_opt': 'a t -> (key -> bool) -> (key * 'a) option
    val map': 'a t -> ('a -> 'b) -> 'b t
    val mapi': 'a t -> (key -> 'a -> 'b) -> 'b t

    val show: (key -> string) -> ('a -> string) -> 'a t -> string

    module List:
    sig
      val find: key -> 'a list t -> 'a list
      val add: key -> 'a -> 'a list t -> 'a list t
      val add_list: (key * 'a) list -> 'a list t -> 'a list t
      val of_list: (key * 'a) list -> 'a list t
    end
  end

  module Make (X: OrderedType) =
  struct
    include Make (X)

    let add_list l m = List.fold_left (fun acc (k, v) -> add k v acc) m l
    let of_list l = add_list l empty

    let update' k m f = update k f m
    let merge' m1 m2 f = merge f m1 m2
    let union' m1 m2 f = union f m1 m2
    let compare' m1 m2 f = compare f m1 m2
    let equal' m1 m2 f = equal f m1 m2
    let iter' m f = iter f m
    let fold' m acc f = fold f m acc
    let for_all' m f = for_all f m
    let exists' m f = exists f m
    let filter' m f = filter f m
    let partition' m f = partition f m
    let find_first' m f = find_first f m
    let find_first_opt' m f = find_first_opt f m
    let find_last' m f = find_last f m
    let find_last_opt' m f = find_last_opt f m
    let map' m f = map f m
    let mapi' m f = mapi f m

    let show show_key show_value map =
      match bindings map with
        | [] ->
            "{}"
        | _ :: _ as l ->
            let show_binding (k, v) = show_key k ^ " => " ^ show_value v in
            "{ " ^ String.concat "; " (List.map show_binding l) ^ " }"

    module List =
    struct
      let find k m = find_opt k m |> default []
      let add k v m = add k (v :: find k m) m
      let add_list l m = List.fold_left (fun acc (k, v) -> add k v acc) m l
      let of_list l = add_list l empty
    end
  end

  module Char = Make (Char)
  module Uchar = Make (Uchar)
  module Int = Make (Int)
  module Int32 = Make (Int32)
  module Int64 = Make (Int64)
  module Float = Make (Float)
  module String = Make (String)
end

module Show =
struct
  let unit () = "()"
  let bool = string_of_bool
  let char x = "'" ^ String.make 1 x ^ "'"
  let uchar x = sf "U+%X" (Uchar.to_int x)
  let int = string_of_int
  let int32 = Int32.to_string
  let int64 = Int64.to_string
  let float = string_of_float
  let string x = "\"" ^ String.escaped x ^ "\""
  let option show_value = function None -> "None" | Some x -> "Some " ^ show_value x

  let list show_item = function
    | [] -> "[]"
    | _ :: _ as l -> "[ " ^ String.concat "; " (List.map show_item l) ^ " ]"

  let array show_item = function
    | [||] -> "[||]"
    | a -> "[| " ^ String.concat "; " (Array.to_list a |> List.map show_item) ^ " |]"

  let pair show_left show_right (left, right) =
    "(" ^ show_left left ^ ", " ^ show_right right ^ ")"

  let tuple_2 = pair

  let tuple_3 show1 show2 show3 (x1, x2, x3) =
    "(" ^ show1 x1 ^
    ", " ^ show2 x2 ^
    ", " ^ show3 x3 ^
    ")"

  let tuple_4 show1 show2 show3 show4 (x1, x2, x3, x4) =
    "(" ^ show1 x1 ^
    ", " ^ show2 x2 ^
    ", " ^ show3 x3 ^
    ", " ^ show4 x4 ^
    ")"

  let char_set = Set.Char.show char
  let char_map show_value = Map.Char.show char show_value

  let uchar_set = Set.Uchar.show uchar
  let uchar_map show_value = Map.Uchar.show uchar show_value

  let int_set = Set.Int.show int
  let int_map show_value = Map.Int.show int show_value

  let int32_set = Set.Int32.show int32
  let int32_map show_value = Map.Int32.show int32 show_value

  let int64_set = Set.Int64.show int64
  let int64_map show_value = Map.Int64.show int64 show_value

  let float_set = Set.Float.show float
  let float_map show_value = Map.Float.show float show_value

  let string_set = Set.String.show string
  let string_map show_value = Map.String.show string show_value
end

module Filename =
struct
  include Filename

  let quote_if_needed s =
    let len = String.length s in
    if len > 0 then
      try
        for i = 0 to len - 1 do
          match s.[i] with
            | 'a'..'z' | 'A'..'Z' | '-' | '_' | '.' | '/' | '~' | '0'..'9' ->
                ()
            | _ ->
                raise Exit
        done;
        s
      with Exit ->
        Filename.quote s
    else
      Filename.quote ""
end
