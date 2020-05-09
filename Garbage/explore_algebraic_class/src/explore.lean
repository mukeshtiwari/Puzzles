import 
  data.vector linear_algebra tactic

namespace algebra

variables {G : Type*} {F : Type*}
  [decidable_eq G]
  [decidable_eq F]
  [add_comm_group G]
  [field F] 
  [vector_space F G]

variables (A B : F)

inductive point  
| Inf_point : point
| Cur_point (x y : F) : 
  y^2 = x^3 + A * x + B -> point



open point
def opp : point A B -> point A B 
| Inf_point := Inf_point 
| (Cur_point x y H) := Cur_point x (-y) begin 
   ring, rw H, ring 
end 


theorem opp_opp : ∀ (p : point A B), opp A B (opp A B p) = p
| Inf_point := by simp [opp]
| (Cur_point x y H) := begin simp [opp] end


lemma sqr_factor : ∀ (y₁ y₂ : F),
   y₁^2 - y₂^2 = 0 → (y₁ - y₂) * (y₁ + y₂) = 0 :=
   begin  
      intros, ring, assumption 
   end 


lemma factor_simp : ∀ (y₁ y₂ : F),
   (y₁ - y₂) * (y₁ + y₂) = 0 → y₁ = y₂ ∨ y₁ = -y₂ :=
   begin 
    intros,
    rw mul_eq_zero at a,
    cases a, 
      left, rwa [sub_eq_zero] at a, 
      right, exact eq_neg_of_add_eq_zero a,  
   end

lemma same_or_opp :  
  ∀ x₁ y₁ x₂ y₂ H₁ H₂ p₁ p₂, x₁ = x₂ → 
  p₁ = Cur_point x₁ y₁ H₁ → p₂ = Cur_point x₂ y₂ H₂ → 
  p₁ = p₂ ∨ p₁ = opp A B p₂ := 
begin 
  intros, subst a, subst p₁, subst p₂,
  have H₃ : y₁^2 - y₂^2 = 0,  
      rw [H₁, H₂], ring,
  have H₄ : y₁ = y₂ ∨ y₁ = -y₂,
       apply factor_simp, apply sqr_factor, 
       assumption,        
  cases H₄,
        left, subst H₄,
        right, simp [opp],  
        assumption,
end









end algebra
