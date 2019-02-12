open Mfa
let type_sigs = [
    ({module_name="erlang"; function_name="+"; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], of_elem TyNumber))));
    ({module_name="erlang"; function_name="-"; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], of_elem TyNumber))));
    ({module_name="erlang"; function_name="/"; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], of_elem TyNumber))));
    ({module_name="erlang"; function_name="*"; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], of_elem TyNumber))));
    ({module_name="erlang"; function_name="=:="; arity=2},
     Type.(of_elem (TyFun ([TyAny; TyAny], Type.bool))));
    ({module_name="erlang"; function_name="=/="; arity=2},
     Type.(of_elem (TyFun ([TyAny; TyAny], Type.bool))));
    ({module_name="erlang"; function_name="=="; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], Type.bool))));
    ({module_name="erlang"; function_name="/="; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], Type.bool))));
    ({module_name="erlang"; function_name=">"; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], Type.bool))));
    ({module_name="erlang"; function_name="<"; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], Type.bool))));
    ({module_name="erlang"; function_name=">="; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], Type.bool))));
    ({module_name="erlang"; function_name="=<"; arity=2},
     Type.(of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], Type.bool))));
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
     Type.(of_elem (TyFun ([of_elem TyNumber], of_elem TyNumber))));
  ]
