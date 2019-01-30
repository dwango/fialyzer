# Derivation Rules in Fialyzer

This file shows derivation rules used in fialyzer.

Our derivation rules are almost same as [the original paper](https://it.uu.se/research/group/hipe/papers/succ_types.pdf)'s one, but extended by remote call, local call, list, etc.

## Derivation Rules

```
A : context (mapping of variable to type)
τ : type
α, β : type variable
x : variable
e : term
a : arity
f : function
p : pattern
g : guard
pg : pattern with guard sequence (i.e., p when g)
fun f/a : local function (f: function name, a: arity)
fun m:f/a : remote function (m: module name, f: function name, a: arity)
```

```
-------------------------- [VAR]
A ∪ {x → τ} ⊢ x : τ, ∅

A ⊢ e1 : τ1, C1 ... en : τn, Cn
------------------------------------------------------ [STRUCT]
A ⊢ {e1, ... , en} : {τ1, ... , τn}, C1 ∧ ... ∧ Cn

A ⊢ e1 : τ1, C1         A ∪ {x → τ1} ⊢ e2 : τ2, C2
---------------------------------------------------- [LET]
A ⊢ let x = e1 in e2 : τ2, C1 ∧ C2

A ∪ {fun xi/ai → αi} ⊢ f1 : τ1, C1 ... fn : τn, Cn      e : τ, C   where ai = length (args fi)
------------------------------------------------------------------------------------------------- [LETREC]
A ⊢ letrec x1 = f1, ... , xn = fn in e : τ, C1 ∧ ... ∧ Cn ∧ C ∧ (τ1 = α1) ∧ ... ∧ (τn = αn)

A ∪ {x1 → α1, ... , xn → αn} ⊢ e : τ, C
---------------------------------------------------------------------- [ABS]
A ⊢ fun(x1, ... , xn) → e : (α1, ... , αn) → τ, C

A ⊢ e : τ, C  e1 : τ1, C1 ... en : τn, Cn
-------------------------------------------------------------------------------------------------------------- [APP]
A ⊢ e(e1, ... , en) : β, (τ = (α1, ... , αn) → α) ∧ (β ⊆ α) ∧ (τ1 ⊆ α1) ∧ ... ∧ (τn ⊆ αn) ∧ C ∧ C1 ∧ ... ∧ Cn

A ⊢ p : τ, Cp     A ⊢ g : τg, Cg
--------------------------------------------- [PAT]
A ⊢ p when g : τ, (τg ⊆ boolean()) ∧ Cp ∧ Cg

                 A ∪ {v → αv | v ∈ Var(p1)} ⊢ pg1 : τpg1, Cpg1,  b1 : τb1, Cb1
                                           ...
A ⊢ e : τ, Ce    A ∪ {v → αv | v ∈ Var(pn)} ⊢ pgn : τpgn, Cpgn,  bn : τbn, Cbn
-------------------------------------------------------------------------------------------------------------------- [CASE]
A ⊢ case e of pg1 → b1; ... pgn → bn end : β, Ce ∧ (C1 ∨ ... ∨ Cn) where Ci = ((β = τbi) ∧ (τ = τpgi) ∧ Cpgi ∧ Cbi)

------------------------------------- [LOCALFUN]
A ∪ {fun f/a → τ} ⊢ fun f/a : τ, ∅

----------------------------------------- if m and f is atom literal, a is non_neg_integer literal [MFA]
A ∪ {fun m:f/a → τ} ⊢ fun m:f/a : τ, ∅

A ⊢ m : τm, Cm   A ⊢ f : τf, Cf   A ⊢ a : τa, Ca
-------------------------------------------------------------------------------------------------- if either m, f, a is not atom literal or non_neg_integer literal [MFAEXPR]
A ⊢ fun m:f/a : β, (β ⊆ any()) ∧ (τm ⊆ atom()) ∧ (τf ⊆ atom()) ∧ (τa ⊆ number()) ∧ Cm ∧ Cf ∧ Ca
```

## Differences from the original paper

The differences from the derivation rules on the original paper are as follows.

- α, β, and τ are clearly distinguished. τ is a type, and α, β are type variables.
- LET is fixed: `e2`, not `e`.
- LETREC is modified: restricted by local functions.
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
