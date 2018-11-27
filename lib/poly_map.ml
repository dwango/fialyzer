open Base

module type Seed = sig
  type t [@@deriving sexp_of]
end

module Make(S : Seed) = struct
  module Comp = struct
    include S
    include Comparator.Make(
                struct
                  include S
                  let compare = Polymorphic_compare.compare
                end)
  end
  type 'v t = 'v Map.M(Comp).t [@@deriving sexp_of]
  let empty = Map.empty(module Comp)
end

module OnMfa = Make(Mfa)
