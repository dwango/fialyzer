# Derivation Rules in Fialyzer

This file shows derivation rules used in fialyzer.

Our derivation rules are almost same as [the original paper](https://it.uu.se/research/group/hipe/papers/succ_types.pdf)'s one, but extended by remote call, local call, list, etc.

## Derivation Rules

```
e ::= v | x | fn | {e, ..., e} | let x = e in e | letrec x = fn, ..., x = fn in e |
      e(e, ..., e) | case e of pg -> e; ...; pg -> e end | fun f/a | fun m:f/a    (term)
v ::= 0 | 'ok' | ...       (constant)
x ::= (snip)               (variable)
fn ::= fun(x, ..., x) -> e (function)
pg ::= p when g; ...; g    (pattern with guard sequence)
p ::= v | x | {e, ..., e}  (pattern)
g ::= v | x | {e, ..., e} | e(e, ..., e)   (guard)
m ::= e                    (module name. a term to be an atom)
f ::= e                    (function name. a term to be an atom)
a ::= e                    (arity. a term to be a non_neg_integer)
τ ::= none() | any () | α | {τ, ..., τ} | (τ, ..., τ) → τ | τ ∪ τ |
      integer() | atom() | 42 | 'ok' ...       (type)
α, β ::= (snip)                                (type variable)
C ::= (τ ⊆ τ) | (C ∧ ... ∧ C) | (C ∨ ... ∨ C)  (constraint)
A ::= A ∪ A | {x → τ, ..., x → τ}              (context. mapping of variable to type)
```

```
-------------------------- [VAR]
A ∪ {x → τ} ⊢ x : τ, ∅

A ⊢ e1 : τ1, C1 ... A ⊢ en : τn, Cn
------------------------------------------------------ [STRUCT]
A ⊢ {e1, ... , en} : {τ1, ... , τn}, C1 ∧ ... ∧ Cn

A ⊢ e1 : τ1, C1         A ∪ {x → τ1} ⊢ e2 : τ2, C2
---------------------------------------------------- [LET]
A ⊢ let x = e1 in e2 : τ2, C1 ∧ C2

A' ⊢ fn1 : τ1, C1 ... A' ⊢ fnn : τn, Cn      A' ⊢ e : τ, C      where A' = A ∪ {xi → αi}
------------------------------------------------------------------------------------------------- [LETREC]
A ⊢ letrec x1 = fn1, ... , xn = fnn in e : τ, C1 ∧ ... ∧ Cn ∧ C ∧ (τ1 = α1) ∧ ... ∧ (τn = αn)

A ∪ {x1 → α1, ... , xn → αn} ⊢ e : τ, C
----------------------------------------------------- [ABS]
A ⊢ fun(x1, ... , xn) → e : (α1, ... , αn) → τ, C

A ⊢ e : τ, C   A ⊢ e1 : τ1, C1 ... A ⊢ en : τn, Cn
-------------------------------------------------------------------------------------------------------------- [APP]
A ⊢ e(e1, ... , en) : β, (τ = (α1, ... , αn) → α) ∧ (β ⊆ α) ∧ (τ1 ⊆ α1) ∧ ... ∧ (τn ⊆ αn) ∧ C ∧ C1 ∧ ... ∧ Cn

A ⊢ p : τ, Cp     A ⊢ g : τg, Cg
--------------------------------------------- [PAT]
A ⊢ p when g : τ, (τg ⊆ boolean()) ∧ Cp ∧ Cg

                 A1 ⊢ pg1 : τpg1, Cpg1    A1 ⊢ b1 : τb1, Cb1
                                       ...
A ⊢ e : τ, Ce    An ⊢ pgn : τpgn, Cpgn    An ⊢ bn : τbn, Cbn      where Ai = A ∪ {v → αv | v ∈ Var(pi)}
-------------------------------------------------------------------------------------------------------------------- [CASE]
A ⊢ case e of pg1 → b1; ... pgn → bn end : β, Ce ∧ (C1 ∨ ... ∨ Cn) where Ci = ((β = τbi) ∧ (τ = τpgi) ∧ Cpgi ∧ Cbi)

------------------------------------- [LOCALFUN]
A ∪ {fun f/a → τ} ⊢ fun f/a : τ, ∅

----------------------------------------- if m and f is atom literal, a is non_neg_integer literal [MFA]
A ∪ {fun m:f/a → τ} ⊢ fun m:f/a : τ, ∅

A ⊢ m : τm, Cm   A ⊢ f : τf, Cf   A ⊢ a : τa, Ca
------------------------------------------------------------------------------------ if neither m, f is atom literal nor a is non_neg_integer literal [MFAEXPR]
A ⊢ fun m:f/a : β, (τm ⊆ atom()) ∧ (τf ⊆ atom()) ∧ (τa ⊆ number()) ∧ Cm ∧ Cf ∧ Ca
```

## Differences from the original paper

The differences from the derivation rules on the original paper are as follows.

- α, β, and τ are clearly distinguished. τ is a type, and α, β are type variables.
- LET is fixed: `e2`, not `e`.
- ABS is modified: `τ` and constrained function are omitted.
- PAT is modified: type of `g` is `boolean()`, not `true`.
- CASE is fixed: `τ`, not `τi`. replaced `p1`...`pn` with `pg1`...`pgn` because these are patterns with guards.
- LOCALFUN is added.
- MFA is added.
- MFAEXPR is added.
- ...and some variables are α-converted for understandability.

## Notes

- In `A ⊢ p : τ, Cp` of PAT rule, `p` is not an expression but a pattern. Therefore, we have to convert `p` to an expression which is the same form of `p`.
  - This is not described in the original paper.
