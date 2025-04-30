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

timesDivRU : (n m : ℕ) .{{_ : NonZero m}} -> n ≤ (n + pred m) / m * m
timesDivRU 0       m = z≤n
timesDivRU (suc n) m =
  let open ≤-Reasoning in begin
    suc n
      ≡⟨ cong suc (m≡m%n+[m/n]*n n m) ⟩
    suc (n % m) + n / m * m
      ≤⟨ +-monoˡ-≤ (n / m * m) (m%n<n n m) ⟩
    m + n / m * m
      ≡⟨ cong (_* m) (cong (_+ n / m) (sym (n/n≡1 m))) ⟩
    (m / m + n / m) * m
      ≡⟨ cong (_* _) (sym (+-distrib-/ m n (+-distrib-/-req n m))) ⟩
    (m + n) / m * m
      ≡⟨ cong (_* _) (cong (_/ _) (cong (_+ _) (sym (suc-pred m)))) ⟩
    (suc (pred m) + n) / m * m
      ≡⟨ cong (_* _) (cong (_/ _) (cong suc (+-comm (pred m) n))) ⟩
    suc (n + pred m) / m * m
  ∎

clogProductRule : (n : ℕ) .{{_ : NonZero n}} -> ⌈log₂ (n * 2) ⌉ ≡ ⌈log₂ n ⌉ + 1
clogProductRule n
  rewrite +-comm ⌈log₂ n ⌉ 1
  rewrite *-comm n 2
  = ⌈log₂2*n⌉≡1+⌈log₂n⌉ n

DivRU : (n d : ℕ) .{{_ : NonZero d}} -> ℕ
DivRU n d = (n + pred d) / d

cancelMulDiv : (n m : ℕ) .{{_ : NonZero m}} -> DivRU (n * m) m ≡ n
cancelMulDiv n (suc m) =
  let open ≡-Reasoning in begin
    (n * suc m + m) / suc m
      ≡⟨ +-distrib-/ (n * suc m) m (lem₁ n m) ⟩
    n * suc m / suc m + m / suc m
      ≡⟨ cong ((n * suc m / _) +_) (m<n⇒m/n≡0 (s≤s ≤-refl)) ⟩
    n * suc m / suc m + 0
      ≡⟨ +-comm _ 0 ⟩
    n * suc m / suc m
      ≡⟨ /-congʳ {suc m} {1 * suc m} {n * suc m} (cong suc (sym (+-comm m 0))) ⟩
    n * suc m / (1 * suc m)
      ≡⟨ m*n/o*n≡m/o n (suc m) 1 ⟩
    n / 1
      ≡⟨ n/1≡n n ⟩
    n
  ∎
  where
    lem₁ : (n m : ℕ) -> (n * suc m) % suc m + m % suc m < suc m
    lem₁ n m = s≤s
      let open ≤-Reasoning in begin
        (n * suc m) % suc m + m % suc m
          ≡⟨ cong ((n * suc m) % suc m +_) (m<n⇒m%n≡m (s≤s ≤-refl)) ⟩
        (n * suc m) % suc m + m
          ≡⟨ cong (_+ m) ([m+kn]%n≡m%n 0 n (suc m)) ⟩
        m
      ∎

divWithRemainder : (n m k : ℕ) .{{_ : NonZero m}} ->
  k ≤ pred m -> (n * m + k) / m ≡ n
divWithRemainder n (suc m) k k≤m-1 =
  let open ≡-Reasoning in begin
    (n * suc m + k) / suc m
      ≡⟨ +-distrib-/ (n * suc m) k (lem₁ n m k≤m-1) ⟩
    n * suc m / suc m + k / suc m
      ≡⟨ cong (n * suc m / suc m +_) (m<n⇒m/n≡0 {k} (s≤s k≤m-1)) ⟩
    n * suc m / suc m + 0
      ≡⟨ +-comm _ 0 ⟩
    n * suc m / suc m
      ≡⟨ /-congʳ {suc m} {1 * suc m} {n * suc m} (cong suc (sym (+-comm m 0))) ⟩
    n * suc m / (1 * suc m)
      ≡⟨ m*n/o*n≡m/o n (suc m) 1 ⟩
    n / 1
      ≡⟨ n/1≡n n ⟩
    n
  ∎
  where
    lem₁ : (n m : ℕ) -> k ≤ m -> n * suc m % suc m + k % suc m < suc m
    lem₁ n m k≤m = s≤s
      let open ≤-Reasoning in begin
        n * suc m % suc m + k % suc m
          ≡⟨ cong (n * suc m % suc m +_) (m≤n⇒m%n≡m k≤m) ⟩
        n * suc m % suc m + k
          ≡⟨ cong (_+ k) ([m+kn]%n≡m%n 0 n (suc m))⟩
        k
          ≤⟨ k≤m ⟩
        m
      ∎

leMaxLeft : (n m k : ℕ) -> n ≤ ((n + m) ⊔ k)
leMaxLeft 0       _ _       = z≤n
leMaxLeft (suc n) m 0       = s≤s (m≤m+n n m)
leMaxLeft (suc n) m (suc k) = s≤s (leMaxLeft n m k)

lessThanMax : (n m k : ℕ) -> n ≤ m -> n ≤ k -> n ≤ m ⊔ k
lessThanMax n m k n≤m n≤k
  rewrite (⊔≡⊔′ m k)
  with m <ᵇ k
... | false = n≤m
... | true  = n≤k

leMaxRight : (n m k : ℕ) -> n ≤ (m ⊔ (n + k))
leMaxRight 0       m       k = z≤n
leMaxRight (suc n) 0       k = s≤s (m≤m+n n k)
leMaxRight (suc n) (suc m) k = s≤s (leMaxRight n m k)

strictlyPositiveDivRu :
  (n m : ℕ) .{{_ : NonZero n}} .{{_ : NonZero m}} ->
  1 ≤ DivRU n m
strictlyPositiveDivRu (suc n) m =
  let open ≤-Reasoning in begin
    1
      ≤⟨ s≤s z≤n ⟩
    suc (n / m)
      ≡⟨  cong (_+ n / m) (sym (n/n≡1 m)) ⟩
    m / m + n / m
      ≡⟨ sym (+-distrib-/ m n (+-distrib-/-req n m)) ⟩
    (m + n) / m
      ≡⟨ cong (_/ _) (cong (_+ _) (sym (suc-pred m)) ) ⟩
    (suc (pred m) + n) / m
      ≡⟨ cong (_/ _) (cong suc (+-comm (pred m) n)) ⟩
    suc (n + pred m) / m
  ∎

euclid3 : (n m k : ℕ) -> n + m ≤ k -> n ≤ k ∸ m
euclid3 n m 0 n+m≤0
  with n+m≡0 <- n≤0⇒n≡0 n+m≤0
  with n≡0 <- m+n≡0⇒m≡0 n n+m≡0
  rewrite n≡0
  = z≤n
euclid3 n 0 (suc k) n+m≤1+k
  rewrite +-comm n 0
  = n+m≤1+k
euclid3 n (suc m) (suc k) n+m≤1+k
  rewrite +-comm n (suc m)
  rewrite +-comm m n
  = euclid3 n m k (s≤s⁻¹ n+m≤1+k)

oneLeCLog2n : (n : ℕ) -> 2 ≤ n -> 1 ≤ ⌈log₂ n ⌉
oneLeCLog2n 0      2≤0 = contradiction n≮0 λ p -> p 2≤0
oneLeCLog2n 1      2≤1 = contradiction n≮0 λ p -> p (s≤s⁻¹ 2≤1)
oneLeCLog2n (2+ n) _   = s≤s z≤n

useLowerLimit :
  (n m k : ℕ) .{{_ : NonZero m }} ->
  n + m ≤ k -> 1 + n ≤ k
useLowerLimit n (suc m) k n+m≤k = ≤-trans 1+n≤n+m n+m≤k
  where
   1+n≤n+m : {n m : ℕ} -> suc n ≤ n + suc m
   1+n≤n+m {n} {m} =
     let open ≤-Reasoning in begin
       suc n
         ≡⟨ +-comm 0 (suc n)  ⟩
       suc n + 0
         ≤⟨ +-monoʳ-≤ (suc n) z≤n ⟩
       suc n + m
         ≡⟨ cong suc (+-comm n m) ⟩
       suc (m + n)
         ≡⟨ +-comm (suc m) n ⟩
       n + suc m
     ∎

minLeq : (n m : ℕ) -> n ⊓ m ≤ m
minLeq n m = m⊓n≤n n m

maxGeqPlus : (n m k : ℕ) -> n ≤ (n ⊔ m) + k
maxGeqPlus n m 0
  rewrite +-comm (n ⊔ m) 0
  = m≤m⊔n n m
maxGeqPlus n m (suc k)
  rewrite +-comm (n ⊔ m) (suc k)
  rewrite +-comm k (n ⊔ m)
  = m≤n⇒m≤1+n (maxGeqPlus n m k)
