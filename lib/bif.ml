open Mfa

let number = Type.(of_elem TyNumber)
let string = Type.(of_elem (TyList (of_elem TyNumber)))
let pid = Type.(of_elem TyPid)
let port = Type.(of_elem TyPort)
let reference = Type.(of_elem TyReference)
let pos_integer = Type.(of_elem TyNumber) (*TODO*)
let non_neg_integer = Type.(of_elem TyNumber) (*TODO*)
let atom s = Type.(of_elem (TySingleton (Atom s)))
let list ty = Type.(of_elem (TyList ty))
let fun_ tys ty = Type.(of_elem (TyFun (tys, ty)))
let fun1 ty1 ty = fun_ [ty1] ty
let fun2 ty1 ty2 ty = fun_ [ty1; ty2] ty
let any_tuple = Type.TyAny (*TODO*)
let tuple ts = Type.(of_elem (TyTuple ts))
let any_map = Type.(of_elem TyAnyMap)

let type_sigs = [
    ({module_name="erlang"; function_name="+"; arity=2},
     Type.(of_elem (TyFun ([number; number], number))));
    ({module_name="erlang"; function_name="-"; arity=2},
     Type.(of_elem (TyFun ([number; number], number))));
    ({module_name="erlang"; function_name="/"; arity=2},
     Type.(of_elem (TyFun ([number; number], number))));
    ({module_name="erlang"; function_name="*"; arity=2},
     Type.(of_elem (TyFun ([number; number], number))));
    ({module_name="erlang"; function_name="=:="; arity=2},
     Type.(of_elem (TyFun ([TyAny; TyAny], Type.bool))));
    ({module_name="erlang"; function_name="=/="; arity=2},
     Type.(of_elem (TyFun ([TyAny; TyAny], Type.bool))));
    ({module_name="erlang"; function_name="=="; arity=2},
     Type.(of_elem (TyFun ([number; number], Type.bool))));
    ({module_name="erlang"; function_name="/="; arity=2},
     Type.(of_elem (TyFun ([number; number], Type.bool))));
    ({module_name="erlang"; function_name=">"; arity=2},
     Type.(of_elem (TyFun ([number; number], Type.bool))));
    ({module_name="erlang"; function_name="<"; arity=2},
     Type.(of_elem (TyFun ([number; number], Type.bool))));
    ({module_name="erlang"; function_name=">="; arity=2},
     Type.(of_elem (TyFun ([number; number], Type.bool))));
    ({module_name="erlang"; function_name="=<"; arity=2},
     Type.(of_elem (TyFun ([number; number], Type.bool))));
    ({module_name="erlang"; function_name="orelse"; arity=2},
     Type.(of_elem (TyFun ([Type.bool; Type.bool], Type.bool))));
    ({module_name="erlang"; function_name="andalso"; arity=2},
     Type.(of_elem (TyFun ([Type.bool; Type.bool], Type.bool))));
    ({module_name="erlang"; function_name="and"; arity=2},
     Type.(of_elem (TyFun ([Type.bool; Type.bool], Type.bool))));
    ({module_name="erlang"; function_name="or"; arity=2},
     Type.(of_elem (TyFun ([Type.bool; Type.bool], Type.bool))));
    ({module_name="erlang"; function_name="xor"; arity=2},
     Type.(of_elem (TyFun ([Type.bool; Type.bool], Type.bool))));
    ({module_name="erlang"; function_name="not"; arity=1},
     Type.(of_elem (TyFun ([Type.bool], Type.bool))));
    ({module_name="erlang"; function_name="abs"; arity=1},
     Type.(of_elem (TyFun ([number], number))));

    (* -------- lists -------------- *)
    ({module_name="lists"; function_name="all"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny TyAny; list TyAny], bool))));
    ({module_name="lists"; function_name="any"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny bool; list TyAny], bool))));
    ({module_name="lists"; function_name="append"; arity=2},
     Type.(of_elem (TyFun ([list TyAny; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="delete"; arity=2},
     Type.(of_elem (TyFun ([TyAny; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="dropwhile"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny bool; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="filter"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny bool; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="flatmap"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny (list TyAny); list TyAny], list TyAny))));
    ({module_name="lists"; function_name="flatten"; arity=1},
     Type.(of_elem (TyFun ([list TyAny], list TyAny))));
    ({module_name="lists"; function_name="foldl"; arity=3},
     Type.(of_elem (TyFun ([fun2 TyAny TyAny TyAny; TyAny; list TyAny], TyAny))));
    ({module_name="lists"; function_name="foldr"; arity=3},
     Type.(of_elem (TyFun ([fun2 TyAny TyAny TyAny; TyAny; list TyAny], TyAny))));
    ({module_name="lists"; function_name="foreach"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny TyAny; list TyAny], atom "ok"))));
    ({module_name="lists"; function_name="keydelete"; arity=3},
     Type.(of_elem (TyFun ([TyAny; pos_integer; list any_tuple], list any_tuple))));
    ({module_name="lists"; function_name="keyfind"; arity=3},
     Type.(of_elem (TyFun ([TyAny; pos_integer; list any_tuple], union_list [any_tuple; atom "false"]))));
    ({module_name="lists"; function_name="keymap"; arity=3},
     Type.(of_elem (TyFun ([fun1 TyAny TyAny; pos_integer; list any_tuple], TyAny))));
    ({module_name="lists"; function_name="keymember"; arity=3},
     Type.(of_elem (TyFun ([TyAny; pos_integer; list any_tuple], bool))));
    ({module_name="lists"; function_name="keymerge"; arity=3},
     Type.(of_elem (TyFun ([pos_integer; list any_tuple; list any_tuple], list any_tuple))));
    ({module_name="lists"; function_name="keyreplace"; arity=4},
     Type.(of_elem (TyFun ([TyAny; pos_integer; list any_tuple; any_tuple], list any_tuple))));
    ({module_name="lists"; function_name="keysearch"; arity=3},
     Type.(of_elem (TyFun ([TyAny; pos_integer; list any_tuple],
                           union_list [tuple [atom "value"; any_tuple]; atom "false"]))));
    ({module_name="lists"; function_name="keysort"; arity=2},
     Type.(of_elem (TyFun ([pos_integer; list any_tuple], list any_tuple))));
    ({module_name="lists"; function_name="last"; arity=1},
     Type.(of_elem (TyFun ([list TyAny], TyAny))));
    ({module_name="lists"; function_name="map"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny TyAny; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="mapfoldl"; arity=3},
     Type.(of_elem (TyFun ([fun2 TyAny TyAny (tuple [TyAny; TyAny]); TyAny; list TyAny], tuple [list TyAny; TyAny]))));
    ({module_name="lists"; function_name="mapfoldr"; arity=3},
     Type.(of_elem (TyFun ([fun2 TyAny TyAny (tuple [TyAny; TyAny]); TyAny; list TyAny], tuple [list TyAny; TyAny]))));
    ({module_name="lists"; function_name="max"; arity=1},
     Type.(of_elem (TyFun ([list TyAny], TyAny))));
    ({module_name="lists"; function_name="member"; arity=2},
     Type.(of_elem (TyFun ([TyAny; list TyAny], bool))));
    ({module_name="lists"; function_name="min"; arity=1},
     Type.(of_elem (TyFun ([list TyAny], TyAny))));
    ({module_name="lists"; function_name="nth"; arity=2},
     Type.(of_elem (TyFun ([pos_integer; list TyAny], TyAny))));
    ({module_name="lists"; function_name="nthtail"; arity=2},
     Type.(of_elem (TyFun ([non_neg_integer; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="partition"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny bool; list TyAny], tuple [list TyAny; list TyAny]))));
    ({module_name="lists"; function_name="reverse"; arity=1},
     Type.(of_elem (TyFun ([list TyAny], list TyAny))));
    ({module_name="lists"; function_name="reverse"; arity=2},
     Type.(of_elem (TyFun ([list TyAny; TyAny], list TyAny))));
    ({module_name="lists"; function_name="sort"; arity=1},
     Type.(of_elem (TyFun ([list TyAny], list TyAny))));
    ({module_name="lists"; function_name="sort"; arity=2},
     Type.(of_elem (TyFun ([fun2 TyAny TyAny bool; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="split"; arity=2},
     Type.(of_elem (TyFun ([non_neg_integer; list TyAny], tuple [list TyAny; list TyAny]))));
    ({module_name="lists"; function_name="splitwith"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny bool; list TyAny], tuple [list TyAny; list TyAny]))));
    ({module_name="lists"; function_name="subtract"; arity=2},
     Type.(of_elem (TyFun ([list TyAny; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="takewhile"; arity=2},
     Type.(of_elem (TyFun ([fun1 TyAny bool; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="unzip"; arity=1},
     Type.(of_elem (TyFun ([list (tuple [TyAny; TyAny])], tuple [list TyAny; list TyAny]))));
    ({module_name="lists"; function_name="unzip3"; arity=1},
     Type.(of_elem (TyFun ([list (tuple [TyAny; TyAny; TyAny])], tuple [list TyAny; list TyAny; list TyAny]))));
    ({module_name="lists"; function_name="usort"; arity=1},
     Type.(of_elem (TyFun ([list TyAny], list TyAny))));
    ({module_name="lists"; function_name="usort"; arity=2},
     Type.(of_elem (TyFun ([fun2 TyAny TyAny bool; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="zip"; arity=2},
     Type.(of_elem (TyFun ([list TyAny; list TyAny], list (tuple [TyAny; TyAny])))));
    ({module_name="lists"; function_name="zip3"; arity=3},
     Type.(of_elem (TyFun ([list TyAny; list TyAny; list TyAny], list (tuple [TyAny; TyAny; TyAny])))));
    ({module_name="lists"; function_name="zipwith"; arity=3},
     Type.(of_elem (TyFun ([fun2 TyAny TyAny TyAny; list TyAny; list TyAny], list TyAny))));
    ({module_name="lists"; function_name="zipwith3"; arity=4},
     Type.(of_elem (TyFun ([fun_ [TyAny; TyAny; TyAny] TyAny; list TyAny; list TyAny; list TyAny], list TyAny))));

    (* -------- maps -------------- *)
    ({module_name="maps"; function_name="from_list"; arity=1},
     Type.(of_elem (TyFun ([list (tuple [TyAny; TyAny])], any_map))));
    ({module_name="maps"; function_name="get"; arity=2},
     Type.(of_elem (TyFun ([TyAny; of_elem TyAnyMap], TyAny))));
    ({module_name="maps"; function_name="is_key"; arity=2},
     Type.(of_elem (TyFun ([TyAny; of_elem TyAnyMap], Type.bool))));
    ({module_name="maps"; function_name="merge"; arity=2},
     Type.(of_elem (TyFun ([any_map; any_map], any_map))));
    ({module_name="maps"; function_name="put"; arity=3},
     Type.(of_elem (TyFun ([TyAny; TyAny; of_elem TyAnyMap], of_elem TyAnyMap))));
    ({module_name="maps"; function_name="size"; arity=1},
     Type.(of_elem (TyFun ([of_elem TyAnyMap], number))));
    ({module_name="maps"; function_name="update"; arity=3},
     Type.(of_elem (TyFun ([TyAny; TyAny; any_map], any_map))));

    (* pid *)
    ({module_name="erlang"; function_name="self"; arity=0},
     Type.(of_elem (TyFun ([], pid))));

    (* port *)
    ({module_name="erlang"; function_name="list_to_port"; arity=1},
     Type.(of_elem (TyFun ([string], port))));

    (* reference *)
    ({module_name="erlang"; function_name="make_ref"; arity=0},
     Type.(of_elem (TyFun ([], reference))));
  ]
