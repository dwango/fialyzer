let type_sigs = [
    (("erlang", "+", 2),
     Ast_intf.(TyFun ([TyNumber; TyNumber], TyNumber)));
    (("erlang", "-", 2),
     Ast_intf.(TyFun ([TyNumber; TyNumber], TyNumber)));
    (("erlang", "/", 2),
     Ast_intf.(TyFun ([TyNumber; TyNumber], TyNumber)));
    (("erlang", "*", 2),
     Ast_intf.(TyFun ([TyNumber; TyNumber], TyNumber)));
  ]
