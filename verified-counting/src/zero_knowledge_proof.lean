import data.zmod.basic data.nat.prime 
 number_theory.quadratic_reciprocity
 tactic.find tactic.omega list_lemma
 elgamal_encryption 


namespace nzkp

variables 
  (p q k : ℕ) (g h pubkey : zmod p) (prikey : zmod q)
  (Hk : 2 ≤ k) (Hp : fact (nat.prime p)) (Hq : fact (nat.prime q))
  (Hdiv : p = q * k + 1)  (H₁ : h ≠ 0) (H₂ : h^k ≠ 1)
  (H₃ : g = h^k) (H₄ : g^prikey.val = pubkey)


def nzkp_decryption_proof (m : zmod p) (c : zmod p × zmod p) (w : zmod q) := true 

end nzkp
