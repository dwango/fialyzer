This file shows derivation rules used in fialyzer.

Our derivation rules are almost same as [the original paper](https://it.uu.se/research/group/hipe/papers/succ_types.pdf)'s one, but extended by remote call, local call, list, etc.

```
A : context (mapping of variable to type)
τ : type
x : variable
e : term
fun f/a : local function (f: function name, a: arity)
fun m:f/a : remote function (m: module name, f: function name, a: arity)
```

```
-------------------------- [VAR]
A ∪ {x → τ} ⊢ x : τ, ∅

A ⊢ e1 : τ1, C1 ... en : τn, Cn
------------------------------------------------------ [STRUCT]
A ⊢ {e1, ... , en} : {τ1, ... , τn}, C1 ∧ ... ∧ Cn

A ⊢ e1 : τ1, C1         A ∪ {x → τ1} ⊢ e : τ2, C2
---------------------------------------------------- [LET]
A ⊢ let x = e1 in e2 : τ2, C1 ∧ C2

A ∪ {fun xi/ai → τi} ⊢ f1 : τ'1, C1 ... fn : τ'n, Cn        e : τ, C
---------------------------------------------------------------------------------------------- [LETREC]
A ⊢ letrec x1 = f1, ... , xn = fn in e : τ, C1 ∧ ... Cn ∧ C ∧ (τ'1 = τ1) ∧ ... ∧ (τ'n = τn)
  where ai = length (args fi)

A ∪ {x1 → τ1, ... , xn → τn} ⊢ e : τe, C
---------------------------------------------------------------------- [ABS]
A ⊢ fun(x1, ... , xn) → e : (τ1, ... , τn) → τe, C

A ⊢ e1 : τ1, C1 ... en : τn, Cn
-------------------------------------------------------------------------------------------------------------- [APP]
A ⊢ e1(e2, ... , en) : β, (τ1 = (α2, ... , αn) → α) ∧ (β ⊆ α) ∧ (τ2 ⊆ α2) ∧ ... ∧ (τn ⊆ αn) ∧ C1 ∧ ... ∧ Cn

A ⊢ p : τ, Cp     A ⊢ g : boolean(), Cg
------------------------------------------ [PAT]
A ⊢ p when g : τ, Cp ∧ Cg

A ∪ {v → τv | v ∈ Var(p1)} ⊢ p1 : α1, Cp1,  b1 : β1, Cb1
....
A ⊢ e : τ, Ce    A ∪ {v → τv | v ∈ Var(pn)} ⊢ pn : αn, Cpn,  bn : βn, Cbn
-------------------------------------------------------------------------------------------------------------------- [CASE]
A ⊢ case e of p1 → b1; ... pn → bn end : β, Ce ∧ (C1 ∨ ... ∨ Cn) where Ci = ((β = βi) ∧ (τi = αi) ∧ Cpi ∧ Cbi)

------------------------------------- [LOCALFUN]
A ∪ {fun f/a → τ} ⊢ fun f/a : τ, ∅

----------------------------------------- if m and f is atom literal, a is non_neg_integer literal [MFA]
A ∪ {fun m:f/a → τ} ⊢ fun m:f/a : τ, ∅

A ⊢ m : τm, Cm   A ⊢ f : τf, Cf   A ⊢ a : τa, Ca
-------------------------------------------------------------------------------------------------- if either m, f, a is not atom literal or non_neg_integer literal [MFAEXPR]
A ⊢ fun m:f/a : τ, (τ ⊆ any()) ∧ (τm ⊆ atom()) ∧ (τf ⊆ atom()) ∧ (τa ⊆ number()) ∧ Cm ∧ Cf ∧ Ca
```

The following derivation rules are differences from the original paper.

- LETREC is modified: restricted by local functions.
- ABS is modified: `τ` and constrained function are omitted.
- PAT is modified: type of `g` is `boolean()`, not `true`.
- LOCALFUN is added.
- MFA is added.
- MFAEXPR is added.
