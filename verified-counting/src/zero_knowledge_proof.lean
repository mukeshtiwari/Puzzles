import data.zmod.basic data.nat.prime 
 number_theory.quadratic_reciprocity
 tactic.find tactic.omega list_lemma
 elgamal_encryption 


namespace nzkp

variables 
  (p q k : ℕ) (g h : zmod p)
  (Hk : 2 ≤ k) (Hp : fact (nat.prime p)) (Hq : fact (nat.prime q))
  (Hdiv : p = q * k + 1)  (H₁ : h ≠ 0) (H₂ : h^k ≠ 1)
  (H₃ : g = h^k)
  
  
variables 
  hash : list (zmod p) -> zmod p 

inductive communication : Type*
| commitment : Π  (t : zmod q), communication
| challenge : Π (t c : zmod q), communication
| response : Π (t c s : zmod q), communication
open communication


/- Add more data to hash function https://tools.ietf.org/html/rfc8235 -/
inductive zkp_transcript (w x : zmod q) (h : zmod p) (Hf : h = g^x.val) : communication p -> Type* 
| commitment_step (t : zmod q)  : t = g^w.val -> zkp_transcript (commitment t)
| challenge_step (t c : zmod q) : c = hash [g, h, t] -> zkp_transcript (commitment t) ->  zkp_transcript (challenge t c) 
| response_step (t c s : zmod q) : s = w + c * x -> zkp_transcript (challenge t c) -> zkp_transcript (response t c s)
open zkp_transcript


/- 
We can construct a sigma protocol (discrete logarithm). 
we can always construct a valid certificate. Moreover, we will prove this 
formally that this function always constructs a valid certificate which checks out
-/
def construct_sigma_protocol (w x : zmod q) (h : zmod p) (H₁ : h = g^x.val) :  
    Σ (t c s : zmod q), zkp_transcript p q g hash w x h H₁ (response t c s)  := 
    let t : zmod q := g^w.val in 
    let c : zmod q := hash [g, h, t] in 
    let s : zmod q := w + c * x in 
    ⟨t, c, s, response_step t c s rfl (challenge_step t c rfl (commitment_step t rfl)) ⟩



/- -/

end nzkp
#eval nzkp.construct_sigma_protocol