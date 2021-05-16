# Mysc

## Scope

This library is a very small extension to the standard library with
values that I tend to define in most of my projects anyway in a `Misc`
module. One goal is that this library can easily be removed from your
project. Using a standard library replacement is an important decision
with consequences that are harder to revert. This is not the goal of
this library, it should be a much more lightweight decision to take.

### Actually Useful

This library tries to introduce values that have actually been defined
in other projects. It does introduce values that have never been used
when it makes sense for consistency, but does not go out of its way to
do so. This makes it easier to maintain.

### No Shadowing

This library does not shadow any value of the standard library. This
means that if you use a value that exists in the standard library you
are guaranteed that this particular value will have its original
meaning. This makes the code easier to read for developers which are
used to the standard library. It also means that using this library
does not prevent you from doing anything that you could before.
It also means that if you do not like a value added by this library,
you can simply ignore it.

This also means that even though nowadays we tend to see, say,
`List.find_opt` as a better alternative to `List.find`, this library
does not define `List.find = List.find_opt`.

This rule also means that values introduced by this library have a
chance of eventually making it to the standard library.

Of course, if a value with the same name is added to the standard
library in the future, this library will shadow it accidentally. It
may be removed in future versions to leave place for the standard
library, although this is not a rule set in stone.

### Avoid Aliases

This library avoids aliases like `let fold = fold_left`. Once again
the goal is to make it easy to revert the decision of using this
library and to make it easier to read for other developers. If a
developer is used to `fold_left`, seeing `fold` instead will just
introduce questions for a rather small benefit. Another example is
that this library does not define `to_list` for sets and maps since
`elements` and `bindings` already exist.

There can be exceptions to this rule for functions where the benefit
is significantly high. For instance, `sf` as an alias to
`Printf.sprintf` is particularly convenient. Compare:
```
declare_something ~name: (Printf.sprintf "%s-%d" base index)
```
with:
```
declare_something ~name: (sf "%s-%d" base index)
```
Because `Printf.sprintf` is so often used, is rather inconvenient to
type, and is long enough that one may need to break and indent the
line, `sf` is worth being an exception to the no-alias rule.

Another example is `default`. Compare:
```
List.find_opt (fun x -> x mod 2 = 0) list |> Option.value ~default: 0
```
with:
```
List.find_opt (fun x -> x mod 2 = 0) list |> default 0
```
Not only `Option.value ~default:` is rather inconvenient to type and
heavy to read, it also does not convey exactly the same intent since
the name `Option.value` focuses on taking the value of an option
whereas `default` focuses on giving a default value.

### Functions as Last Arguments

This library defines "primed" versions of several functions such as
`List.iter'` where the order of arguments is changed to make the
function the last argument. This is very convenient when combined
with `@@` as follows:
```
List.iter' list1 @@ fun item1 ->
List.iter' list2 @@ fun item2 ->
print_string (item1 ^ item2)
```

### Could Be Included in Stdlib

New values in this library try to be consistent with the Stdlib.
This increases the chance that they can be added to the Stdlib.
For instance, it adds `List.fold_left'` instead of naming it `List.fold`.
The latter would have my preference but would not be consistent
and would thus have less chance of being included in the Stdlib.

### Not Worth A Separate Library

New values in this library do not justify a new library on their own.
For instance, one could say: Base64 is very useful in most of my
projects. But it is easy to justify a Base64 library that does just
Base64. This library collects a bunch of unrelated additions that are
so small that one would laugh at the idea of making a library out of
them, not only because the library would be very small, but also
because it would be more inconvenient to add it as a dependency than
to just rewrite them locally.

### Sets and Maps

This library provides instantiations of `Set.Make` and `Map.Make` for
base modules such as `Int` and `String`. Most projects eventually
define them and different applications of `Set.Make` and `Map.Make`
are not compatible, so it is particularly convenient to define those
once and for all. Those modules are defined as submodules of `Set` and
`Map` and not in the "pervasive" part of the library because one
cannot add `String_map` etc. to `Stdlib.Pervasives` since it would
create a dependency cycle. Modules `Map.String` thus has at least a
chance at being adopted into the standard library.

This library also defines a submodule `List` in the result of
`Map.Make`. Maps are often used to associate lists of values to keys
(kind of like `Hashtbl`), and it is convenient to consider that if a
key is not present in a map, it is equivalent to being associated to
the empty list. It is also convenient to have an `add` function that
adds to the list instead of replacing the whole list.
