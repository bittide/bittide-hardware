-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- Proofs of the type level properties claimed by
-- 'Data.Constraint.Nat.Extra'. Checked with agda-2.6.4.1 and
-- agda-stdlib-1.2.0.

open import Data.Bool.Base                        using (T)
open import Agda.Builtin.Bool                     using (true; false)
open import Agda.Builtin.Unit                     using (tt)
open import Data.Empty                            using (⊥-elim)
open import Data.Nat.Base
open import Data.Nat.DivMod
open import Data.Nat.Logarithm
open import Data.Nat.Properties
open import Function.Base                         using (_∘_)
open import Relation.Binary.PropositionalEquality
open import Function.Identity.Effectful           using (Identity)
open import Relation.Nullary.Negation             using (contradiction)

+-distrib-/-req : (n m : ℕ) .{{_ : NonZero m}} -> m % m + n % m < m
+-distrib-/-req n (suc m) = s≤s
  let open ≤-Reasoning in begin
     suc m % suc m + n % suc m
       ≡⟨ cong (_+ (n % suc m)) (n%n≡0 (suc m)) ⟩
     n % suc m
       ≤⟨ s≤s⁻¹ (m%n<n n (suc m)) ⟩
     m
   ∎

OneMore : ℕ -> ℕ
OneMore 0       = 0
OneMore (suc _) = 1

isOne :
  (n m : ℕ) .{{_ : NonZero n}} .{{_ : NonZero m}} ->
  n ≤ m -> n / m + OneMore (n % m) ≡ 1
isOne n m n≤m
  with n ≤ᵇ m in n≤ᵇm?
... | false = ⊥-elim (subst Identity (cong T n≤ᵇm?) (≤⇒≤ᵇ n≤m))
... | true
    with n ≡ᵇ m in n≡ᵇm?
...   | true
        rewrite ≡ᵇ⇒≡ n m (subst T (sym n≡ᵇm?) tt)
        rewrite cong (_+ OneMore (m % m)) (n/n≡1 m)
        rewrite cong suc (cong OneMore (n%n≡0 m))
        = refl
...   | false
        with n≢m <- subst T (n≡ᵇm?) ∘ ≡⇒≡ᵇ n m
        with n<m <- ≤∧≢⇒< n≤m n≢m
        rewrite m<n⇒m%n≡m n<m
        rewrite m<n⇒m/n≡0 n<m
        with suc _ <- n
        = refl

oneMore :
  (n m : ℕ) .{{_ : NonZero n}} .{{_ : NonZero m}} ->
  1 ≤ n / m + OneMore (n % m)
oneMore n m
  with n ≤ᵇ m in n≤ᵇm?
... | true
      rewrite isOne n m (≤ᵇ⇒≤ n m (subst T (sym n≤ᵇm?) tt))
      = s≤s z≤n
... | false
      with n≰m <- subst T (n≤ᵇm?) ∘ ≤⇒≤ᵇ
      rewrite m/n≡1+[m∸n]/n {n} {m} (≰⇒≥ n≰m)
      = s≤s z≤n

SatSubZero : ℕ -> ℕ -> ℕ
SatSubZero 0 _ = 0
SatSubZero n m
  with n ≤ᵇ m
... | true  = 0
... | false = n ∸ m

satSubZeroMin : (n m : ℕ) -> SatSubZero n m + n ⊓ m ≡ n
satSubZeroMin 0       m       = refl
satSubZeroMin (suc n) 0       = cong suc (+-comm n 0)
satSubZeroMin (suc n) (suc m)
  rewrite +-comm (SatSubZero n m) (n ⊓ m)
  rewrite +-comm (n ⊓ m) (SatSubZero (suc n) (suc m))
  rewrite ⊓≡⊓′ (suc n) (suc m)
  with n <ᵇ m in n<ᵇm? | n <ᵇ suc m in n<ᵇ1+m?
... | false | false
      rewrite +-comm (n ∸ m) (suc m)
      = cong suc (m+[n∸m]≡n {m} {n} (≮⇒≥ (subst T n<ᵇm? ∘ <⇒<ᵇ)))
... | false | true
      with n≤m <- s≤s⁻¹ (<ᵇ⇒< n (suc m) (subst T (sym n<ᵇ1+m?) tt))
      = cong suc (sym (≤∧≮⇒≡ n≤m (subst T n<ᵇm? ∘ <⇒<ᵇ)))
... | true  | false
      with n≤m <- <⇒≤ (<ᵇ⇒< n m (subst T (sym n<ᵇm?) tt))
      = ⊥-elim (subst T n<ᵇ1+m? (<⇒<ᵇ (s≤s n≤m)))
... | true  | true
      = refl
