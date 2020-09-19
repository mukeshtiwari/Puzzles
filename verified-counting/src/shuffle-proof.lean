import data.zmod.basic data.nat.prime 
 number_theory.quadratic_reciprocity
 tactic.find tactic.omega list_lemma



namespace encryption
    
variables 
  (p q k : ℕ) (g h pubkey : zmod p) (prikey : zmod q)
  (Hk : 2 ≤ k) (Hp : fact (nat.prime p)) (Hq : fact (nat.prime q))
  (Hdiv : p = q * k + 1)  (H₁ : h ≠ 0) (H₂ : h^k ≠ 1)
  (H₃ : g = h^k) (H₄ : g^prikey.val = pubkey)

/- ElGamal Encryption -/
def elgamal_enc (m : zmod p) (r : zmod q) := 
  (g ^ r.val, g ^ m.val * pubkey ^ r.val)


/- ElGamal Decryption -/
def elgamal_dec (c : zmod p × zmod p) := 
  c.2 * (c.1 ^ prikey.val)⁻¹  
 
/- ElGamal Reencryption -/ 
def elgamal_reenc (c : zmod p × zmod p) (r : zmod q) :=  
  (c.1 * g ^ r.val, c.2 * pubkey ^ r.val)

/- Multiplication of two ciphertext -/
def ciphertext_mult (c d : zmod p × zmod p) :=  
  (c.1 * d.1, c.2 * d.2)

 
/- Encryption of a ballot (vector of length n) -/
def elgamal_enc_ballot : list (zmod p) → list (zmod q) → list (zmod p × zmod p) 
| [] [] := []
| [] (r :: rs) := [] 
| (m :: ms) [] := []
| (m :: ms) (r :: rs) := 
    (elgamal_enc p q g pubkey m r) :: elgamal_enc_ballot ms rs 

/- Encryption of a ballot (vector of length n) 
def vector_elgamal_enc {n : ℕ} :  
  vector (zmod p) n -> vector (zmod q) n -> vector (zmod p × zmod p) n  
  | ⟨ms, Hm⟩  ⟨rs, Hr⟩ := 
    ⟨list.zip_with (elgamal_enc p q g pubkey) ms rs, 
    begin
      have Ht : list.length ms = list.length rs :=  
      begin rw [Hm, Hr] end,
      rw <- Hm, apply zip_with_len_l, exact Ht
    end⟩ -/

def elgamal_dec_ballot : list (zmod p × zmod p) → list (zmod p) 
| [] :=  [] 
| (c :: cs) := elgamal_dec p q prikey c :: elgamal_dec_ballot cs 

/- Decryption of a ballot (vector of length n) 
def vector_elgamal_dec {n : ℕ} :  
  vector (zmod p × zmod p) n -> vector (zmod p) n  
  | ⟨cs, Hc⟩  := 
    ⟨list.map (elgamal_dec p q prikey) cs, 
    begin 
      rw <- Hc, apply map_with_len_l, 
    end⟩ -/

def elgamal_reenc_ballot : list (zmod p × zmod p) → list (zmod q) → list (zmod p × zmod p)
| [] [] := []
| [] (r :: rs) := [] 
| (c :: cs) [] := []
| (c :: cs) (r :: rs) :=  
  (elgamal_reenc p q g pubkey c r) :: elgamal_reenc_ballot cs rs 

/- Reencryption of ballot (vector of length n)
def vector_elgamal_reenc {n : ℕ} :  
  vector (zmod p × zmod p) n -> vector (zmod q) n -> vector (zmod p × zmod p) n  
  | ⟨cs, Hc⟩  ⟨rs, Hr⟩ := 
    ⟨list.zip_with (elgamal_reenc p q g pubkey) cs rs, 
    begin 
      have Ht : list.length cs = list.length rs :=  
      begin rw [Hc, Hr] end,
      rw <- Hc, apply zip_with_len_l, exact Ht
    end⟩ -/

def ciphertext_mult_ballot : list (zmod p × zmod p) → list (zmod p × zmod p) → list (zmod p × zmod p)
| [] [] := []
| [] (c₂ :: cs₂) := [] 
| (c₁ :: cs₁) [] := []
| (c₁ :: cs₁) (c₂:: cs₂) := 
        (ciphertext_mult p c₁ c₂) :: ciphertext_mult_ballot cs₁ cs₂ 

        
/- Component wise multiplication of two ballot of same length (vector of length n )
def vector_ciphertext_mult {n : ℕ} :  
  vector (zmod p × zmod p) n -> vector (zmod p × zmod p) n -> vector (zmod p × zmod p) n  
  | ⟨cs₁, Hc₁⟩  ⟨cs₂, Hc₂⟩ := 
    ⟨list.zip_with (ciphertext_mult p) cs₁  cs₂, 
    begin 
      have Ht : list.length cs₁ = list.length cs₂ :=  
      begin rw [Hc₁, Hc₂] end,
      rw <- Hc₁, apply zip_with_len_l, exact Ht,
    end⟩-/

def raise_message_to_generator : list (zmod p) → list (zmod p)
| [] := [] 
| (m :: ms) := (g ^ m.val) :: raise_message_to_generator ms 

/- Decrypting additive ElGamal gives g^m 
def raise_message_to_generator {n : ℕ} : vector (zmod p) n → vector (zmod p) n
| ⟨ms, Hm⟩ := ⟨list.map (λ (m : zmod p), g ^ m.val) ms, 
  begin
    rw map_with_len_l, exact Hm
  end⟩ -/


/- Compute finally tally, which is multiplying all the ballots 
   component wise. In final count, these ballots will be 
   passed through a (formally verified) mix-net with zero-knowledge 
   proof -/


 
/- proof that g is a generator of the group -/
include Hp Hq Hdiv H₁ H₂ H₃ H₄ 
lemma generator_proof : g ^ q = 1 := 
begin
  rw [H₃, ← pow_mul, mul_comm],
  have H : p - 1 = q * k := nat.pred_eq_of_eq_succ Hdiv,
  rw ← H, exact zmod.pow_card_sub_one_eq_one H₁, 
end  

/- correctness property of encryption and decryption -/
theorem elgamal_enc_dec_identity : ∀ m r c, 
  c = elgamal_enc p q g pubkey m r →
  elgamal_dec p q prikey c = g ^ m.val := 
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

/- re-encryption correctness -/
theorem elgamal_reenc_correct : ∀ r r₁ m c₁ c₂, c₁ = elgamal_enc p q g pubkey m r → 
  c₂ = elgamal_reenc p q g pubkey c₁ r₁ →  elgamal_dec p q prikey c₂ = g ^ m.val :=
begin
  intros  r r₁ m c₁ c₂ Hc₁ Hc₂, symmetry,
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
  apply pow_ne_zero, rw H₃, apply pow_ne_zero, exact H₁,
end  


theorem additive_homomorphic_property : ∀ c d m₁ m₂ r₁ r₂,
 c = elgamal_enc p q g pubkey m₁ r₁ ->
 d = elgamal_enc p q g pubkey m₂ r₂ -> 
 (g ^ (r₁.val + r₂.val), g ^ (m₁.val + m₂.val) * 
 pubkey ^ (r₁.val + r₂.val)) = ciphertext_mult p c d := 
begin 
  unfold elgamal_enc ciphertext_mult, 
  intros c d m₁ m₂ r₁ r₂ Hc Hd, rw [Hc, Hd], simp,
  have Ht₁ : g ^ (r₁.val + r₂.val) = g ^ r₁.val * g ^ r₂.val := 
    pow_add g r₁.val r₂.val,
  have Ht₂ : g ^ (m₁.val + m₂.val) * pubkey ^ (r₁.val + r₂.val) =  
    g ^ m₁.val * pubkey ^ r₁.val * (g ^ m₂.val * pubkey ^ r₂.val) :=  
    begin rw [pow_add, pow_add], simp, ring end,
  exact and.intro Ht₁ Ht₂,
end 

theorem ballot_elgamal_enc_dec_identity : ∀ (ms : list (zmod p)) (rs : list (zmod q)) cs,
  list.length ms = list.length rs → cs = elgamal_enc_ballot p q g pubkey ms rs → 
  elgamal_dec_ballot p q prikey cs = raise_message_to_generator p g ms 
  | [] [] := 
    begin 
      simp [elgamal_enc_ballot, elgamal_dec_ballot, 
        raise_message_to_generator], 
    end 
  | [] (rsh :: rst) := λ cs Hl, begin contradiction, end
  | (msh :: mst) [] := λ cs Hl, begin contradiction, end
  | (msh :: mst) (rsh :: rst) := λ cs Hl Hcs,
    begin 
      rw Hcs, cases cs with csh cst, contradiction, 
      simp [elgamal_enc_ballot] at Hcs, 
      simp [elgamal_enc_ballot, elgamal_dec_ballot, 
        raise_message_to_generator], 
      split, apply elgamal_enc_dec_identity _ _ _ _ _ _ _ Hp Hq Hdiv H₁ H₂ H₃ H₄,
      refl, cases Hcs, 
      specialize (ballot_elgamal_enc_dec_identity mst rst cst _ Hcs_right),
      rw ← Hcs_right, assumption, simp [list.length] at Hl, exact Hl,  
    end 

    
theorem ballot_elgamal_reenc_correct : ∀ (ms : list (zmod p)) rs rs₁ cs₁ cs₂, 
  list.length ms = list.length rs → list.length rs = list.length rs₁ →
  cs₁ = elgamal_enc_ballot p q g pubkey ms rs → 
  cs₂ = elgamal_reenc_ballot p q g pubkey cs₁ rs₁ → 
  elgamal_dec_ballot p q prikey cs₂ = raise_message_to_generator p g ms 
  | [] [] [] := begin intros _ _ Hl₁ Hl₂,
      simp[elgamal_enc_ballot], intro Hc₁, 
      rw Hc₁, simp[elgamal_reenc_ballot], 
      intros Hc₂, rw Hc₂, simp[elgamal_dec_ballot, raise_message_to_generator]
     end
  | [] [] (r₁ :: rs₁) := begin intros, contradiction end
  | [] (r :: rs) [] := begin intros, contradiction end
  | [] (r :: rs) (r₁ :: rs₁) := begin intros, contradiction end
  | (m :: ms) [] [] := begin intros, contradiction end
  | (m :: ms) [] (r₁ :: rs₁) := begin intros, contradiction end
  | (m :: ms) (r :: rs) [] := begin intros, contradiction end 
  | (m :: ms) (r :: rs) (r₁ :: rs₁) := begin 
      intros _ _ Ht₁ Ht₂ Ht₃ Ht₄, 
      cases cs₁ with csh₁ cst₁, 
      contradiction, cases cs₂ with csh₂ cst₂,
      contradiction, simp [elgamal_reenc_ballot] at Ht₄,
      simp [elgamal_enc_ballot] at Ht₃, 
      simp [elgamal_dec_ballot, raise_message_to_generator],
      split, cases Ht₄ with Hl Hr, cases Ht₃ with Htl Htr, 
      apply elgamal_reenc_correct; assumption, 
      specialize (ballot_elgamal_reenc_correct ms rs rs₁ cst₁ cst₂ _ _  Ht₃.2 Ht₄.2),
      assumption, simp[list.length] at Ht₁, assumption,  
      simp[list.length] at Ht₂, assumption,
  end 

/- proof the correctness of above functions -/
/- shuffle  -/
/- zero knowlege proof -/
/- run it and submit it to ccs -/

end encryption

namespace honest_decryption_zkp 



end honest_decryption_zkp