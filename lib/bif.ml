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
  ]
