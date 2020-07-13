import data.nat.basic
 data.rat.basic data.rat.order
 tactic algebra.pi_instances

def neg_fn (μ : ℕ → ℚ) : Prop :=
  ∀ (c : ℕ), ∃ (n₀ : ℕ),
    ∀ (n : ℕ), n₀ ≤ n → μ n * (n ^ c) < 1


theorem sum_neg_fn_is_neg_fn :
  ∀ (μ₁ μ₂ : ℕ → ℚ), neg_fn μ₁ → neg_fn μ₂ → neg_fn (μ₁ + μ₂) :=
begin
  intros μ₁ μ₂ H₁ H₂ c,
  cases H₁ (c + 2) with n₁ Hn₁,
  cases H₂ (c + 2) with n₂ Hn₂,
  use max n₁ (max n₂ 2), intros n Hn,
  repeat {rw max_le_iff at Hn},
  rcases Hn with ⟨Ht₁, Ht₂, Ht₃⟩,
  specialize Hn₁ n Ht₁,
  specialize Hn₂ n Ht₂,
  rw pi.add_apply,
  rw [pow_add, ←mul_assoc] at Hn₁ Hn₂,
  have npos : 0 < (↑n : ℚ), {norm_cast, omega},
  have hspq : 0 < (↑n : ℚ)^2 := pow_pos npos 2,
  rw ← lt_div_iff hspq at Hn₁ Hn₂,
  have Ht₄ : (4 : ℚ) ≤ (↑n : ℚ)^2, 
    {norm_cast, exact nat.pow_le_pow_of_le_left Ht₃ 2},
  have Ht₅ : 1/(↑n : ℚ)^2 ≤ 1/(4 : ℚ), 
    {exact div_le_div_of_le_left zero_le_one (by linarith) Ht₄},
  linarith,
end

theorem fn_bouded_by_neg_fn_is_neg : 
  ∀ (μ₁ μ₂ : ℕ → ℚ), (∀ x : ℕ, μ₁ x ≤ μ₂ x) → neg_fn μ₂ → neg_fn μ₁ :=
begin
  unfold neg_fn, intros μ₁ μ₂ Hf Hn c,
  cases Hn c with n₀ Hn₀, 
  use n₀, intros n Hn₁,  specialize Hn₀ n Hn₁, 
  specialize Hf n, 
  have Ht₁ : 0 ≤ (↑n : ℚ)^c, {norm_cast, exact zero_le (n ^ c)},
  have Ht₂ : μ₁ n * (↑n : ℚ)^c ≤ μ₂ n * (↑n : ℚ)^c := mul_mono_nonneg Ht₁ Hf, 
  linarith
end 

theorem exp_2_neg : neg_fn (λ t : ℕ, 2^(0-t)) := 
begin 
  unfold neg_fn, intro c,
  use c * c, intros n Hc,
  sorry    
end
