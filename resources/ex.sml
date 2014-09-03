val foo = fn (x :int) => x

val bar = fn (x, y) => x + y

val baz = if true then
	      1
	  else
	      if true then
		  2
	      else
		  3

fun foo_2 x = x

fun let_ex _ =
    let
	val x = 1
    in
	x
    end

fun coerce_bool_str x =
    case x of
	"true" => true
      | "false" => false
      | _ => false
