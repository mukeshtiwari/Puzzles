import data.zmod.basic data.nat.prime 
 number_theory.quadratic_reciprocity
 tactic.find tactic.omega data.vector
 list_lemma



namespace shuffle 


variables 
  (p q k : ℕ) (g h pubkey : zmod p) (prikey : zmod q)
  [Hk : 2 ≤ k] [Hp : fact (nat.prime p)] [Hq : fact (nat.prime q)]
  [Hdiv : p = q * k + 1]  [H₁ : h ≠ 0] [H₂ : h^k ≠ 1]
  [H₃ : g = h^k] [H₄ : g^prikey.val = pubkey]


def elgamal_enc (m : zmod p) (r : zmod q) := 
  (g^r.val, g^m.val * pubkey^r.val)

def elgamal_dec (c : zmod p × zmod p) := 
  c.2 * (c.1^prikey.val)⁻¹  
 
def elgamal_reenc (c : zmod p × zmod p) (r : zmod q) :=  
  (c.1 * g^r.val, c.2 * pubkey^r.val)

def ciphertext_mult (c d : zmod p × zmod p) :=  
  (c.1 * d.1, c.2 * d.2)

def vector_elegamal_enc {n : ℕ} :  
  vector (zmod p) n -> vector (zmod q) n -> vector (zmod p × zmod p) n  
  | ⟨ms, Hm⟩  ⟨rs, Hr⟩ := 
    ⟨list.zip_with (elgamal_enc p q g pubkey) ms rs, 
    begin
      have Ht : list.length ms = list.length rs :=  
      begin rw [Hm, Hr] end,
      rw <- Hm, apply zip_with_len_l, exact Ht
    end⟩

def vector_elegamal_dec {n : ℕ} :  
  vector (zmod p × zmod p) n -> vector (zmod p) n  
  | ⟨cs, Hc⟩  := 
    ⟨list.map (elgamal_dec p q prikey) cs, 
    begin 
      rw <- Hc, apply map_with_len_l, 
    end⟩

def vector_elegamal_reenc {n : ℕ} :  
  vector (zmod p × zmod p) n -> vector (zmod q) n -> vector (zmod p × zmod p) n  
  | ⟨cs, Hc⟩  ⟨rs, Hr⟩ := 
    ⟨list.zip_with (elgamal_reenc p q g pubkey) cs rs, 
    begin 
      have Ht : list.length cs = list.length rs :=  
      begin rw [Hc, Hr] end,
      rw <- Hc, apply zip_with_len_l, exact Ht
    end⟩


def vector_ciphertext_mult {n : ℕ} :  
  vector (zmod p × zmod p) n -> vector (zmod p × zmod p) n -> vector (zmod p × zmod p) n  
  | ⟨cs₁, Hc₁⟩  ⟨cs₂, Hc₂⟩ := 
    ⟨list.zip_with (ciphertext_mult p) cs₁  cs₂, 
    begin 
      have Ht : list.length cs₁ = list.length cs₂ :=  
      begin rw [Hc₁, Hc₂] end,
      rw <- Hc₁, apply zip_with_len_l, exact Ht,
    end⟩


/- g  is a generator of the group -/
include Hp Hq Hdiv H₁ H₂ H₃ H₄ 
lemma generator_proof : g^q = 1 := 
begin
  rw [H₃, ← pow_mul, mul_comm],
  have H : p - 1 = q * k := nat.pred_eq_of_eq_succ Hdiv,
  rw ← H, exact zmod.pow_card_sub_one_eq_one H₁, 
end  

/- correctness property of encryption and decryption -/
theorem elgamal_enc_dec_identity : ∀ m r c, 
  c = elgamal_enc p q g pubkey m r →
  elgamal_dec p q prikey c = g^m.val := 
begin
  unfold elgamal_enc elgamal_dec, simp, 
  intros m r c Hc, rw [Hc, ←H₄, ←pow_mul, ←pow_mul, mul_assoc],
  have Hc : prikey.val * r.val = r.val * prikey.val := 
    mul_comm (zmod.val prikey) (zmod.val r), 
  have Hd : g ≠ 0 :=  begin rw H₃, 
    exact pow_ne_zero k H₁ end, 
  rw [Hc, mul_inv_cancel], simp, 
  exact pow_ne_zero _ Hd, 
end


theorem elgamal_reenc_correct : ∀ r r₁ m c₁ c₂, c₁ = elgamal_enc p q g pubkey m r → 
  c₂ = elgamal_reenc p q g pubkey c₁ r₁ → g^m.val = elgamal_dec p q prikey c₂ :=
begin
  intros  r r₁ m c₁ c₂ Hc₁ Hc₂, 
  rw [Hc₂, Hc₁], unfold elgamal_reenc elgamal_enc elgamal_dec, simp, 
  rw [←H₄, tactic.ring.pow_add_rev g (zmod.val r) (zmod.val r₁)],
  rw @tactic.ring_exp.pow_e_pf_exp _ _ (g ^ prikey.val) g prikey.val r.val _ rfl rfl,
  rw @tactic.ring_exp.pow_e_pf_exp _ _ (g ^ prikey.val) g prikey.val r₁.val _ rfl rfl,
  rw @tactic.ring_exp.pow_e_pf_exp _ _ (g ^ (r.val + r₁.val)) g (r.val + r₁.val) prikey.val _ rfl rfl,
  have Ha : g ^ m.val * g ^ (prikey.val * r.val) * g ^ (prikey.val * r₁.val) * 
    (g ^ ((r.val + r₁.val) * prikey.val))⁻¹ = g ^ m.val * 
    (g ^ (prikey.val * r.val) * g ^ (prikey.val * r₁.val)) * (g ^ ((r.val + r₁.val) * prikey.val))⁻¹, ring, 
  rw [Ha, tactic.ring.pow_add_rev g (prikey.val * r.val) (prikey.val * r₁.val)], clear Ha,
  have Hb : g ^ (prikey.val * r.val + prikey.val * r₁.val) = g ^ (prikey.val * (r.val + r₁.val)), ring, 
  rw [Hb, mul_comm prikey.val (r.val + r₁.val), mul_assoc,  mul_inv_cancel], ring, 
  apply pow_ne_zero, rw H₃, apply pow_ne_zero, assumption
end  


theorem additive_homomorphic_property : forall c d m₁ m₂ r₁ r₂,
 c = elgamal_enc p q g pubkey m₁ r₁ ->
 d = elgamal_enc p q g pubkey m₂ r₂ -> 
 (g^(r₁.val + r₂.val), g^(m₁.val + m₂.val) * 
 pubkey^(r₁.val + r₂.val)) = ciphertext_mult p c d := 
begin 
  unfold elgamal_enc ciphertext_mult, 
  intros c d m₁ m₂ r₁ r₂ Hc Hd, rw [Hc, Hd], simp,
  have Ht₁ : g ^ (r₁.val + r₂.val) = g ^ r₁.val * g ^ r₂.val := 
    pow_add g r₁.val r₂.val,
  have Ht₂ : g ^ (m₁.val + m₂.val) * pubkey ^ (r₁.val + r₂.val) =  
    g ^ m₁.val * pubkey ^ r₁.val * (g ^ m₂.val * pubkey ^ r₂.val) :=  
    begin rw [pow_add, pow_add], simp, ring end,
  exact and.intro Ht₁ Ht₂
end 

/- proof the correctness of above functions -/
/- shuffle  -/
/- zero knowlege proof -/
/- run it and submit it to ccs -/

end shuffle