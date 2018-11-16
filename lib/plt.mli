(**
   plt file reader and writer (OTP-21.1.1)
   @see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/dialyzer/src/dialyzer_plt.erl>

   TODO:
   - read info
   - read types
   - read callbacks
   - read exported_types
   - writer
 *)

open Base
module Format = Caml.Format

(**
{v
-record(plt, {info      :: ets:tid(), %% {mfa() | integer(), ret_args_types()}
              types     :: ets:tid(), %% {module(), erl_types:type_table()}
              contracts :: ets:tid(), %% {mfa(), #contract{}}
              callbacks :: ets:tid(), %% {module(),
                                      %%  [{mfa(),
                                      %%  dialyzer_contracts:file_contract()}]
              exported_types :: ets:tid() %% {module(), sets:set()}
             }).
v}
@see <https://github.com/erlang/otp/blob/OTP-21.1.1/lib/dialyzer/src/dialyzer_plt.erl#L78>
 *)
type t
[@@deriving sexp_of]

val of_file : string -> (t, exn) Result.t
