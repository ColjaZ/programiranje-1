-- Strukture:

-- (A x B) ^ C <=> A ^ C x B ^ C
def eksponent (A B C : Type) (f : C → Prod A B) : Prod (C → A) (C → B) :=
  ⟨
    fun c => (f c).1,
    fun c => (f c).2
  ⟩
def eksponent_prop (A B C : Prop) (f : C → A ∧ B) : (C → A) ∧ (C → B) :=
  ⟨
    sorry,
    sorry
  ⟩

#check And.intro
def eksponent_prop_s_taktikami (A B C : Prop) (f : C → A ∧ B) : (C → A) ∧ (C → B) :=
  by
    apply And.intro
    · intro h
      have h1 := f h
      exact h1.left

    · intro h
      exact (f h).right




-- ------------------------------
-- Logika

theorem eq1 {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  by
    apply Iff.intro
    · intro h
      apply And.intro
      · exact h.right
      · exact h.left
    · intro h
      apply And.intro
      · exact h.right
      · exact h.left



theorem eq2 {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
  by
    apply Iff.intro
    · intro h
      cases h with
      | inl ha =>
        apply Or.inr
        exact ha
      | inr hb =>
        apply Or.inl
        exact hb
    · intro h
      cases h with
      | inl h1 =>
        apply Or.inr
        exact h1
      | inr h2 =>
        apply Or.inl
        exact h2


theorem eq3 {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) :=
  by sorry

theorem eq4 {A B C : Prop} : (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C)) :=
 sorry

theorem eq5 {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) :=
  sorry

theorem eq6 {A B C : Prop} : (B ∨ C) → A ↔ (B → A) ∧ (C → A) :=
  sorry

theorem eq7 {A B C : Prop} : C → (A ∧ B) ↔ (C → A) ∧ (C → B) :=
  sorry


-- ------------------------------
-- Enakosti naravnih števil (z uporabo `calc`)

#check Nat.pow_two
#check Nat.add_mul
#check Nat.mul_comm
#check Nat.add_assoc
#check Nat.mul_add
theorem kvadrat_dvoclenika {a b : Nat} : (a + b)^2 = a^2 + 2 * a * b + b^2 :=
  by
    calc
      (a + b)^2
      _ = (a + b) * (a + b) := by rw [Nat.pow_two]
      _ = a * (a + b) + b * (a + b) := by rw [Nat.add_mul]
      _ = a*a + a*b + b * (a + b) := by rw [Nat.mul_add]
      _ = a*a + a*b + (b*a + b*b) := by rw [Nat.mul_add]
      _ = a^2 + a*b + (b*a + b^2) := by repeat rw [Nat.pow_two]
      _ = a^2 + (a*b + (b*a + b^2)) := by rw [Nat.add_assoc]
      _ = a^2 + ((a*b + b*a) + b^2) := by rw [Nat.add_assoc]
      _ = a^2 + ((a*b + a*b) + b^2) := by rw [Nat.mul_comm]
      _ = a^2 + (2*(a*b) + b^2) := by rw [Nat.two_mul]
      _ = a^2 + 2 * (a * b) + b^2 := by rw [Nat.add_assoc]
      _ = a^2 + 2 * a * b + b^2 := by rw [Nat.mul_assoc]


theorem vsota_eksponent_produkta {a b c d : Nat} : (a * b)^(c + d) = (a^c)*(a^d)*(b^c)*(b^d) :=
  by
    calc
      (a * b)^(c + d)
      _ = a^c * a^d * b^c * b^d := by sorry
