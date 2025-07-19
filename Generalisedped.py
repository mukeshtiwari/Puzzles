import random
import hashlib
from sympy import nextprime, isprime 

class SchnorrGroup:
    def __init__(self, bits=512):
        """
        Initialize a Schnorr group (subgroup of prime order q in Z_p^*)
        :param bits: Bit length of prime p (default 512 bits for demonstration)
        """
        self.bits = bits
        self.p = self._generate_safe_prime(bits)
        self.q = (self.p - 1) // 2  # q is prime for safe prime p
        self.g = self._find_generator()

    def _generate_safe_prime(self, bits):
        """Generate a safe prime p = 2q + 1 where q is prime"""
        q = nextprime(1 << (bits - 1))
        while True:
            q = nextprime(q)
            p = 2 * q + 1
            if bits <= p.bit_length() and isprime(q) and isprime(p):
                return p

    def _find_generator(self):
        """Find a generator g for the subgroup of order q"""
        while True:
            h = random.randint(2, self.p - 2)
            g = pow(h, 2, self.p)
            if g != 1:
                return g

    def __repr__(self):
        return f"SchnorrGroup(p={self.p}, q={self.q}, g={self.g})"

class ElGamal:
    def __init__(self, group):
        self.group = group
        self.sk = random.randint(1, group.q - 1)  # secret key
        self.pk = pow(group.g, self.sk, group.p)  # public key

    def encrypt(self, m, r):
        """Encrypt message m (must be in subgroup)"""
        c1 = pow(self.group.g, r, self.group.p)
        c2 = (m * pow(self.pk, r, self.group.p)) % self.group.p
        return c1, c2

def hash_to_challenge(*args, q):
    """Hash arbitrary arguments to a challenge in [0, q-1]"""
    h = hashlib.sha256()
    for arg in args:
        if isinstance(arg, list):
            for item in arg:
                h.update(str(item).encode())
        else:
            h.update(str(arg).encode())
    return int(h.hexdigest(), 16) % q

def generate_disjunctive_proof(group, pk, ciphertext, messages, b, r):
    """
    Generate proof that ciphertext encrypts one of the messages
    :param group: SchnorrGroup instance
    :param pk: Public key (h)
    :param ciphertext: Tuple (c1, c2)
    :param messages: List of n messages in subgroup
    :param b: Index of true message
    :param r: Randomness used in encryption
    :return: Proof tuple (t1_list, t2_list, s_list, c_list)
    """
    p, q, g = group.p, group.q, group.g
    c1, c2 = ciphertext
    n = len(messages)
    
    t1_list = [0] * n
    t2_list = [0] * n
    s_list = [0] * n
    c_list = [0] * n
    r_b = None  # Will store randomness for true branch
    
    # Generate commitments for each branch
    for i in range(n):
        if i == b:
            # Real branch: generate random exponent
            r_b = random.randint(1, q - 1)
            t1_list[i] = pow(g, r_b, p)
            t2_list[i] = pow(pk, r_b, p)
        else:
            # Simulated branch: random s_i and c_i
            s_i = random.randint(1, q - 1)
            c_i = random.randint(1, q - 1)
            s_list[i] = s_i
            c_list[i] = c_i
            # t1_i = g^{s_i} * c1^{-c_i} mod p
            t1_list[i] = (pow(g, s_i, p) * pow(c1, -c_i, p)) % p
            # t2_i = h^{s_i} * (c2 * m_i^{-1})^{-c_i} mod p
            t2_list[i] = (pow(pk, s_i, p) * pow(c2 * pow(messages[i], -1, p), -c_i, p)) % p
    
    # Compute global challenge
    c_total = hash_to_challenge(pk, c1, c2, messages, t1_list, t2_list, q=q)
    
    # Compute challenge for true branch
    c_sum_others = sum(c_list[i] for i in range(n) if i != b) % q
    c_b = (c_total - c_sum_others) % q
    c_list[b] = c_b
    
    # Compute response for true branch
    s_b = (r_b + c_b * r) % q
    s_list[b] = s_b
    
    return t1_list, t2_list, s_list, c_list

def verify_disjunctive_proof(group, pk, ciphertext, messages, proof):
    """
    Verify disjunctive proof
    :param group: SchnorrGroup instance
    :param pk: Public key (h)
    :param ciphertext: Tuple (c1, c2)
    :param messages: List of n messages
    :param proof: Tuple (t1_list, t2_list, s_list, c_list)
    :return: True if proof valid, False otherwise
    """
    p, q = group.p, group.q
    c1, c2 = ciphertext
    t1_list, t2_list, s_list, c_list = proof
    n = len(messages)
    
    # Verify challenge sum
    c_total = hash_to_challenge(pk, c1, c2, messages, t1_list, t2_list, q=q)
    if sum(c_list) % q != c_total:
        return False
    
    # Verify each branch
    for i in range(n):
        s_i, c_i = s_list[i], c_list[i]
        
        # Check g^{s_i} == t1_i * c1^{c_i} mod p
        left1 = pow(group.g, s_i, p)
        right1 = (t1_list[i] * pow(c1, c_i, p)) % p
        if left1 != right1:
            return False
        
        # Check h^{s_i} == t2_i * (c2 * m_i^{-1})^{c_i} mod p
        left2 = pow(pk, s_i, p)
        msg_term = (c2 * pow(messages[i], -1, p)) % p
        right2 = (t2_list[i] * pow(msg_term, c_i, p)) % p
        if left2 != right2:
            return False
    
    return True

# =====================
# Example Usage
# =====================
if __name__ == "__main__":
    # Initialize Schnorr group (small bits for demonstration)
    group = SchnorrGroup(bits=5)
    print(f"Group parameters: p={group.p}, q={group.q}, g={group.g}")
    
    # Initialize ElGamal cryptosystem
    elgamal = ElGamal(group)
    print(f"Public key (h): {elgamal.pk}")
    
    # Generate test messages in the subgroup
    messages = [pow(group.g, i, group.p) for i in [10, 20, 30, 40]]
    print(f"Messages: {messages}")
    
    # Encrypt the 2nd message (index 1)
    true_idx = 1
    r_val = random.randint(1, group.q - 1)
    ciphertext = elgamal.encrypt(messages[true_idx], r_val)
    print(f"Ciphertext: {ciphertext}")
    
    # Generate proof
    proof = generate_disjunctive_proof(
        group, elgamal.pk, ciphertext, messages, true_idx, r_val
    )
    print("\nProof generated")
    print(f"Proof: {proof}")

    # Verify proof (should be valid)
    valid = verify_disjunctive_proof(group, elgamal.pk, ciphertext, messages, proof)
    print(f"Proof valid: {valid}")
    
    # Test with incorrect messages (should fail)
    wrong_messages = [pow(group.g, i, group.p) for i in [15, 25, 35, 45]]
    valid_wrong = verify_disjunctive_proof(
        group, elgamal.pk, ciphertext, wrong_messages, proof
    )
    print(f"Proof with wrong messages: {valid_wrong}")
    
    # Test with tampered proof (should fail)
    t1_list, t2_list, s_list, c_list = proof
    c_list[0] = (c_list[0] + 1) % group.q  # Tamper with challenge
    tampered_proof = (t1_list, t2_list, s_list, c_list)
    valid_tampered = verify_disjunctive_proof(
        group, elgamal.pk, ciphertext, messages, tampered_proof
    )
    print(f"Tampered proof: {valid_tampered}")
