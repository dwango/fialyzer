open Mfa
let type_sigs = [
    ({module_name="erlang"; function_name="+"; arity=2},
     Ast_intf.(TyFun ([TyNumber; TyNumber], TyNumber)));
    ({module_name="erlang"; function_name="-"; arity=2},
     Ast_intf.(TyFun ([TyNumber; TyNumber], TyNumber)));
    ({module_name="erlang"; function_name="/"; arity=2},
     Ast_intf.(TyFun ([TyNumber; TyNumber], TyNumber)));
    ({module_name="erlang"; function_name="*"; arity=2},
     Ast_intf.(TyFun ([TyNumber; TyNumber], TyNumber)));
  ]
