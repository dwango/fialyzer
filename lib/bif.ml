open Mfa
let type_sigs = [
    ({module_name="erlang"; function_name="+"; arity=2},
     Type.(TyFun ([TyNumber; TyNumber], TyNumber)));
    ({module_name="erlang"; function_name="-"; arity=2},
     Type.(TyFun ([TyNumber; TyNumber], TyNumber)));
    ({module_name="erlang"; function_name="/"; arity=2},
     Type.(TyFun ([TyNumber; TyNumber], TyNumber)));
    ({module_name="erlang"; function_name="*"; arity=2},
     Type.(TyFun ([TyNumber; TyNumber], TyNumber)));
  ]
