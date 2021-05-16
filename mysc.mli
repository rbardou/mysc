(** Small extension to the standard library. *)

(** Give a default value to an optional value.

    [default x] is the same as [Option.value ~default: x]. *)
val default: 'a -> 'a option -> 'a

(** Print a line on standard output, with [Printf] formatting. *)
val echo: ('a, unit, string, unit) format4 -> 'a

(** Same as [Printf.sprintf]. *)
val sf: ('a, unit, string) format -> 'a

(** Same as [int_of_float], just like [float] is the same as [float_of_int]. *)
val int: float -> int

(** Round a float number to the nearest integer.

    Values that are exactly in-between two integers are rounded up if positive,
    down if negative: [round 1.5] is [2] and [round (-1.5) is (-2)]. *)
val round: float -> int

(** Same as [(mod)], but always return a positive integer.

    For instance, [(-1) % 10] is [9]. *)
val (%): int -> int -> int

(** Same as [(mod_float)], but always return a positive float.

    For instance, [(-1.) %. 10.] is [9.]. *)
val (%.): float -> float -> float

(** Open a file for writing, do something, and close the file.

    The file is closed even if an exception was raised.

    @raise Sys_error if the file cannot be opened. *)
val with_open_out: string -> (out_channel -> 'a) -> 'a

(** Open a file for reading, do something, and close the file.

    The file is closed even if an exception was raised.

    @raise Sys_error if the file cannot be opened. *)
val with_open_in: string -> (in_channel -> 'a) -> 'a

(** Same as [Filename.concat]. *)
val (//): string -> string -> string

(** Read the whole contents of a file given its filename.

    [buffer_size] is the initial size of the internal buffer.
    It is also how many bytes we try to read at a time.
    It does not have to be as big as the file.
    Default value is 1024.

    @raise Sys_error if the file cannot be read. *)
val read_file: ?buffer_size: int -> string -> string

module Option:
sig
  (** Option values. *)

  include module type of struct include Stdlib.Option end

  (** [map' o f = map f o] *)
  val map': 'a option -> ('a -> 'b) -> 'b option

  (** [fold' acc o f = fold ~none: acc ~some: f o] *)
  val fold': 'a -> 'b option -> ('b -> 'a) -> 'a

  (** [iter' o f = iter f o] *)
  val iter': 'a option -> ('a -> unit) -> unit

  (** [equal' o f = equal f o] *)
  val equal': 'a option -> 'a option -> ('a -> 'a -> bool) -> bool

  (** [compare' o f = compare f o] *)
  val compare': 'a option -> 'a option -> ('a -> 'a -> int) -> int
end

module List:
sig
  (** List operations. *)

  include module type of struct include Stdlib.List end

  (** {2 Making Lists} *)

  (** Make a list from a range of integer.

      For instance, [range 5 8] is [[ 5; 6; 7; 8 ]]. *)
  val range: int -> int -> int list

  (** {2 Functions as Last Arguments} *)

  (** The functions below are equivalent to corresponding functions
      of the Stdlib except that the function argument comes last.
      For instance, [iter' l f = iter f l]. This makes it possible
      to write [List.iter' list @@ fun item -> ...] which is easier
      to chain and introduces less parentheses and indentation levels. *)

  (** [iter' l f = iter f l] *)
  val iter': 'a list -> ('a -> unit) -> unit

  (** [iteri' l f = iteri f l] *)
  val iteri': 'a list -> (int -> 'a -> unit) -> unit

  (** [map' l f = map f l] *)
  val map': 'a list -> ('a -> 'b) -> 'b list

  (** [mapi' l f = mapi f l] *)
  val mapi': 'a list -> (int -> 'a -> 'b) -> 'b list

  (** [rev_map' l f = rev_map f l] *)
  val rev_map': 'a list -> ('a -> 'b) -> 'b list

  (** [filter_map' l f = filter_map f l] *)
  val filter_map': 'a list -> ('a -> 'b option) -> 'b list

  (** [concat_map' l f = concat_map f l] *)
  val concat_map': 'a list -> ('a -> 'b list) -> 'b list

  (** [fold_left' acc l f = fold_left f acc l] *)
  val fold_left': 'a -> 'b list -> ('a -> 'b -> 'a) -> 'a

  (** [fold_right' l acc f = fold_right f l acc] *)
  val fold_right': 'a list -> 'b -> ('a -> 'b -> 'b) -> 'b

  (** [iter2' l1 l2 f = iter2 f l1 l2] *)
  val iter2': 'a list -> 'b list -> ('a -> 'b -> unit) -> unit

  (** [map2' l1 l2 f = map2 f l1 l2] *)
  val map2': 'a list -> 'b list -> ('a -> 'b -> 'c) -> 'c list

  (** [rev_map2' l1 l2 f = rev_map2 f l1 l2] *)
  val rev_map2': 'a list -> 'b list -> ('a -> 'b -> 'c) -> 'c list

  (** [fold_left2' acc l1 l2 f = fold_left2 f acc l1 l2] *)
  val fold_left2': 'a -> 'b list -> 'c list -> ('a -> 'b -> 'c -> 'a) -> 'a

  (** [fold_right2' l1 l2 acc f = fold_right2 f l1 l2 acc] *)
  val fold_right2': 'a -> 'b list -> 'c list -> ('b -> 'c -> 'a -> 'a) -> 'a

  (** [for_all' l f = for_all f l] *)
  val for_all': 'a list -> ('a -> bool) -> bool

  (** [exists' l f = exists f l] *)
  val exists': 'a list -> ('a -> bool) -> bool

  (** [for_all2' l1 l2 f = for_all2 f l1 l2] *)
  val for_all2': 'a list -> 'b list -> ('a -> 'b -> bool) -> bool

  (** [exists2' l1 l2 f = exists2 f l1 l2] *)
  val exists2': 'a list -> 'b list -> ('a -> 'b -> bool) -> bool

  (** [find' l f = find f l] *)
  val find': 'a list -> ('a -> bool) -> 'a

  (** [find_opt' l f = find_opt f l] *)
  val find_opt': 'a list -> ('a -> bool) -> 'a option

  (** [find_map' l f = find_map f l] *)
  val find_map': 'a list -> ('a -> 'b option) -> 'b option

  (** [filter' l f = filter f l] *)
  val filter': 'a list -> ('a -> bool) -> 'a list

  (** [find_all' l f = find_all f l] *)
  val find_all': 'a list -> ('a -> bool) -> 'a list

  (** [partition' l f = partition f l] *)
  val partition': 'a list -> ('a -> bool) -> 'a list * 'a list

  (** [sort' l f = sort f l] *)
  val sort': 'a list -> ('a -> 'a -> int) -> 'a list

  (** [stable_sort' l f = stable_sort f l] *)
  val stable_sort': 'a list -> ('a -> 'a -> int) -> 'a list

  (** [fast_sort' l f = fast_sort f l] *)
  val fast_sort': 'a list -> ('a -> 'a -> int) -> 'a list

  (** [sort_uniq' l f = sort_uniq f l] *)
  val sort_uniq': 'a list -> ('a -> 'a -> int) -> 'a list

  (** [merge' l1 l2 f = merge f l1 l2] *)
  val merge': 'a list -> 'a list -> ('a -> 'a -> int) -> 'a list
end

module Array:
sig
  (** Array operations. *)

  include module type of struct include Stdlib.Array end

  (** [iter' a f = iter f a] *)
  val iter': 'a array -> ('a -> unit) -> unit

  (** [iteri' a f = iteri f a] *)
  val iteri': 'a array -> (int -> 'a -> unit) -> unit

  (** [map' a f = map f a] *)
  val map': 'a array -> ('a -> 'b) -> 'b array

  (** [mapi' a f = mapi f a] *)
  val mapi': 'a array -> (int -> 'a -> 'b) -> 'b array

  (** [fold_left' acc a f = fold_left f acc a] *)
  val fold_left': 'a -> 'b array -> ('a -> 'b -> 'a) -> 'a

  (** [fold_right' a acc f = fold_right f a acc] *)
  val fold_right': 'a array -> 'b -> ('a -> 'b -> 'b) -> 'b

  (** [iter2' a1 a2 f = iter2 f a1 a2] *)
  val iter2': 'a array -> 'b array -> ('a -> 'b -> unit) -> unit

  (** [map2' a1 a2 f = map2 f a1 a2] *)
  val map2': 'a array -> 'b array -> ('a -> 'b -> 'c) -> 'c array

  (** [for_all' a f = for_all f a] *)
  val for_all': 'a array -> ('a -> bool) -> bool

  (** [exists' a f = exists f a] *)
  val exists': 'a array -> ('a -> bool) -> bool

  (** [sort' a f = sort f a] *)
  val sort': 'a array -> ('a -> 'a -> int) -> unit

  (** [stable_sort' a f = stable_sort f a] *)
  val stable_sort': 'a array -> ('a -> 'a -> int) -> unit

  (** [fast_sort' a f = fast_sort f a] *)
  val fast_sort': 'a array -> ('a -> 'a -> int) -> unit
end

module Set:
sig
  (** Sets over ordered types. *)

  include module type of struct include Stdlib.Set end

  module type S =
  sig
    include Stdlib.Set.S

    (** Add a list of elements to a set. *)
    val add_list: elt list -> t -> t

    (** [iter' s f = iter f s] *)
    val iter': t -> (elt -> unit) -> unit

    (** [map' s f = map f s] *)
    val map': t -> (elt -> elt) -> t

    (** [fold' s acc f = fold f s acc] *)
    val fold': t -> 'a -> (elt -> 'a -> 'a) -> 'a

    (** [for_all' s f = for_all f s] *)
    val for_all': t -> (elt -> bool) -> bool

    (** [exists' s f = exists f s] *)
    val exists': t -> (elt -> bool) -> bool

    (** [filter' s f = filter f s] *)
    val filter': t -> (elt -> bool) -> t

    (** [partition' s f = partition f s] *)
    val partition': t -> (elt -> bool) -> t * t

    (** [find_first' s f = find_first f s] *)
    val find_first': t -> (elt -> bool) -> elt

    (** [find_first_opt' s f = find_first_opt f s] *)
    val find_first_opt': t -> (elt -> bool) -> elt option

    (** [find_last' s f = find_last f s] *)
    val find_last': t -> (elt -> bool) -> elt

    (** [find_last_opt' s f = find_last_opt f s] *)
    val find_last_opt': t -> (elt -> bool) -> elt option

    (** Convert a set to a string, given a function that converts an element to a string.

        The result is formatted as follows:
        - it is surrounded by braces ([{] and [}]);
        - elements are separated by semi-colons;
        - and spaces are used to separate all of these except at the left of semi-colons
          and for the empty set which is printed as ["{}"]. *)
    val show: (elt -> string) -> t -> string

    (** [show' s f = show f s] *)
    val show': t -> (elt -> string) -> string
  end

  module Make (X: OrderedType): S with type elt = X.t

  (** {2 Base Types} *)

  module Char: S with type elt = char
  module Uchar: S with type elt = Uchar.t
  module Int: S with type elt = int
  module Int32: S with type elt = int32
  module Int64: S with type elt = int64
  module Float: S with type elt = float
  module String: S with type elt = string
end

module Map:
sig
  (** Association tables over ordered types. *)

  include module type of struct include Stdlib.Map end

  module type S =
  sig
    (** Output signature of the functor Map.Make. *)

    include Stdlib.Map.S

    (** Convert an association list to a map. *)
    val of_list: (key * 'a) list -> 'a t

    (** Add a list of bindings to a map.

        Same as folding [add] over the list. *)
    val add_list: (key * 'a) list -> 'a t -> 'a t

    (** [update' k m f = update k f m] *)
    val update': key -> 'a t -> ('a option -> 'a option) -> 'a t

    (** [merge' m1 m2 f = merge f m1 m2] *)
    val merge': 'a t -> 'b t -> (key -> 'a option -> 'b option -> 'c option) -> 'c t

    (** [union' m1 m2 f = union f m1 m2] *)
    val union': 'a t -> 'a t -> (key -> 'a -> 'a -> 'a option) -> 'a t

    (** [compare' m1 m2 f = compare f m1 m2] *)
    val compare': 'a t -> 'a t -> ('a -> 'a -> int) -> int

    (** [equal' m1 m2 f = equal f m1 m2] *)
    val equal': 'a t -> 'a t -> ('a -> 'a -> bool) -> bool

    (** [iter' m f = iter f m] *)
    val iter': 'a t -> (key -> 'a -> unit) -> unit

    (** [fold' m acc f = fold f m acc] *)
    val fold': 'a t -> 'b -> (key -> 'a -> 'b -> 'b) -> 'b

    (** [for_all' m f = for_all f m] *)
    val for_all': 'a t -> (key -> 'a -> bool) -> bool

    (** [exists' m f = exists f m] *)
    val exists': 'a t -> (key -> 'a -> bool) -> bool

    (** [filter' m f = filter f m] *)
    val filter': 'a t -> (key -> 'a -> bool) -> 'a t

    (** [partition' m f = partition f m] *)
    val partition': 'a t -> (key -> 'a -> bool) -> 'a t * 'a t

    (** [find_first' m f = find_first f m] *)
    val find_first': 'a t -> (key -> bool) -> key * 'a

    (** [find_first_opt' m f = find_first_opt f m] *)
    val find_first_opt': 'a t -> (key -> bool) -> (key * 'a) option

    (** [find_last' m f = find_last f m] *)
    val find_last': 'a t -> (key -> bool) -> key * 'a

    (** [find_last_opt' m f = find_last_opt f m] *)
    val find_last_opt': 'a t -> (key -> bool) -> (key * 'a) option

    (** [map' m f = map f m] *)
    val map': 'a t -> ('a -> 'b) -> 'b t

    (** [mapi' m f = mapi f m] *)
    val mapi': 'a t -> (key -> 'a -> 'b) -> 'b t

    (** Convert a map to a string, given functions that converts keys and values.

        The result is formatted as follows:
        - it is surrounded by braces ([{] and [}]);
        - bindings are separated by semi-colons;
        - keys are separated from values by arrows [=>];
        - and spaces are used to separate all of these except at the left of semi-colons
          and for the empty map which is printed as ["{}"]. *)
    val show: (key -> string) -> ('a -> string) -> 'a t -> string

    module List:
    sig
      (** Operations on maps where values are lists. *)

      (** Such a map where a key [k] is bound to a list [l] is seen
          as a map where [k] is associated to each value of [l].
          In other words, each item of the list is a different binding.
          Also, if a key is not present, it is equivalent to being associated
          to the empty list. *)

      (** Get all bindings associated to a key.

          This cannot fail: if the key is absent from the map,
          this function returns the empty list. *)
      val find: key -> 'a list t -> 'a list

      (** Add a binding to a map where values are lists.

          If the key is already associated to some values, the new value
          is added at the beginning of the list. Else, the key becomes
          associated to a singleton list. *)
      val add: key -> 'a -> 'a list t -> 'a list t

      (** Add a list of bindings to a map where values are lists.

          Same as folding [add] over the list. *)
      val add_list: (key * 'a) list -> 'a list t -> 'a list t

      (** Convert an association list to a map where values are lists.

          If the same key appears several times in the list,
          it becomes associated to the list of all of the associated values,
          in reverse order from the original list. *)
      val of_list: (key * 'a) list -> 'a list t
    end
  end

  module Make (X: OrderedType): S with type key = X.t

  (** {2 Base Types} *)

  module Char: S with type key = char
  module Uchar: S with type key = Uchar.t
  module Int: S with type key = int
  module Int32: S with type key = int32
  module Int64: S with type key = int64
  module Float: S with type key = float
  module String: S with type key = string
end

module Show:
sig
  (** String conversion. *)

  (** This module defines a small set of functions to help display
      OCaml values with no line breaks nor indentation.
      It is intended for quick debugging.

      Example 1:
      [
        Show.(tuple_3 bool (option int) (list string))
          (true, Some 42, ["hello"; "world"])
      ]
      results in the string ["(true, Some 42, [ \"hello\"; \"world\" ])"].

      Example 2:
      [
        Show.(string_map int_set)
          (Map.String.of_list [ "hello", Set.Int.empty; "world", Set.Int.of_list [ 42; 69 ] ])
      ]
      results in the string ["{ \"hello\" => {}; \"world\" => { 42; 69 } }"]. *)

  (** The constant function that returns ["()"]. *)
  val unit: unit -> string

  (** Same as [string_of_bool]. *)
  val bool: bool -> string

  (** Same as [string_of_int]. *)
  val int: int -> string

  (** Same as [string_of_float]. *)
  val float: float -> string

  (** Convert a string to a string in double quotes.

      Special characters are escaped using OCaml conventions. *)
  val string: string -> string

  (** Convert an optional value to a string given a function to convert the value.

      The result is either ["None"] or ["Some <value>"] where [<value>] is the result
      of the given function. In particular this means that
      [option (option unit) (Some (Some ()))] results in ["Some Some ()"], which is
      not valid OCaml syntax but is nevertheless unambiguous if you only use
      functions of this module. *)
  val option: ('a -> string) -> 'a option -> string

  (** Convert a list to a string given a function to convert the items.

      The result is formatted using OCaml syntax:
      - it is surrounded by brackets (['['] and [']']);
      - items are separated by semi-colons;
      - and spaces are used to separate all of these except at the left of semi-colons
        and for the empty list which is printed as ["[]"]. *)
  val list: ('a -> string) -> 'a list -> string

  (** Convert a array to a string given a function to convert the items.

      The result is formatted using OCaml syntax:
      - it is surrounded by brackets and pipes (['[|'] and ['|]']);
      - items are separated by semi-colons;
      - and spaces are used to separate all of these except at the left of semi-colons
        and for the empty list which is printed as ["[||]"]. *)
  val array: ('a -> string) -> 'a array -> string

  (** Convert a pair to a string given functions to convert the values.

      The result is formatted using OCaml syntax:
      - it is surrounded by parentheses;
      - and the values are separated by commas followed by a space character. *)
  val pair: ('a -> string) -> ('b -> string) -> 'a * 'b -> string

  (** Same as [pair]. *)
  val tuple_2: ('a -> string) -> ('b -> string) -> 'a * 'b -> string

  (** Same as [pair] but for tuples of 3 values. *)
  val tuple_3: ('a -> string) -> ('b -> string) -> ('c -> string) -> 'a * 'b * 'c -> string

  (** Same as [pair] but for tuples of 4 values. *)
  val tuple_4:
    ('a -> string) ->
    ('b -> string) ->
    ('c -> string) ->
    ('d -> string) ->
    'a * 'b * 'c * 'd -> string

  (** Convert a character set to a string.

      This is the same as [Set.Char.show char]. *)
  val char_set: Set.Char.t -> string

  (** Convert a character map to a string given a function to convert values.

      This is the same as [Map.Char.show char]. *)
  val char_map: ('a -> string) -> 'a Map.Char.t -> string

  (** Convert a unicode character set to a string.

      This is the same as [Set.Uchar.show uchar]. *)
  val uchar_set: Set.Uchar.t -> string

  (** Convert a unicode character map to a string given a function to convert values.

      This is the same as [Map.Uchar.show uchar]. *)
  val uchar_map: ('a -> string) -> 'a Map.Uchar.t -> string

  (** Convert an integer set to a string.

      This is the same as [Set.Int.show int]. *)
  val int_set: Set.Int.t -> string

  (** Convert an integer map to a string given a function to convert values.

      This is the same as [Map.Int.show int]. *)
  val int_map: ('a -> string) -> 'a Map.Int.t -> string

  (** Convert a 32-bit integer set to a string.

      This is the same as [Set.Int32.show int32]. *)
  val int32_set: Set.Int32.t -> string

  (** Convert a 32-bit integer map to a string given a function to convert values.

      This is the same as [Map.Int32.show int32]. *)
  val int32_map: ('a -> string) -> 'a Map.Int32.t -> string

  (** Convert a 64-bit integer set to a string.

      This is the same as [Set.Int64.show int64]. *)
  val int64_set: Set.Int64.t -> string

  (** Convert a 64-bit integer map to a string given a function to convert values.

      This is the same as [Map.Int64.show int64]. *)
  val int64_map: ('a -> string) -> 'a Map.Int64.t -> string

  (** Convert a float set to a string.

      This is the same as [Set.Float.show float]. *)
  val float_set: Set.Float.t -> string

  (** Convert a float map to a string given a function to convert values.

      This is the same as [Map.Float.show float]. *)
  val float_map: ('a -> string) -> 'a Map.Float.t -> string

  (** Convert a string set to a string.

      This is the same as [Set.String.show string]. *)
  val string_set: Set.String.t -> string

  (** Convert a string map to a string given a function to convert values.

      This is the same as [Map.String.show string]. *)
  val string_map: ('a -> string) -> 'a Map.String.t -> string
end

module Filename:
sig
  (** Operations on file names. *)

  include module type of struct include Filename end

  (** Same as [quote], but only actually quote if needed.

      Quotes are considered to be needed if the string is empty or if one
      of its characters is not part of the following set:
      ['a'..'z' | 'A'..'Z' | '-' | '_' | '.' | '/' | '~' | '0'..'9']

      For instance, [quote_if_needed "hello"] is ["hello"], not ["'hello'"]. *)
  val quote_if_needed: string -> string
end
