from Crypto.Util.number import getPrime, getRandomRange, isPrime, inverse

#https://crypto.stackexchange.com/questions/96042/faking-pedersen-commitment/117695#117695

def generate_safe_prime(bits):
  while True:
    q = getPrime(bits)
    p = 2 * q + 1
    if isPrime(p):
        return p, q

# print(generate_safe_prime(64))

def generate(param):
  p = param[0]
  q = param[1]
  g = param[2]
  h = param[3]
  s = param[4]
  return p,q,g,h,s

class verifier:
  def setup(self, bits):
    p, q = generate_safe_prime(bits)
    g = 4
    assert pow(g, q, p) == 1
    s = getRandomRange(1, q-1)
    #print("Secret value:\t",s)
    h = pow(g,s,p)
    param = (p,q,g,h,s)
    return param

  def open(self, param, c, m, r):
    p,q,g,h,s = generate(param)
    res = (pow(g,m,p) * pow(h,r,p)) % p
    return (c == res)

        
class prover: 
  def commit(self, param, m):
    p,q,g,h,s= generate(param)
    r = getRandomRange(1, q-1)
    c = (pow(g,m,p) * pow(h,r,p)) % p
    return c, r

  # I am going to open it to a random arbitrary message m2
  def fake_message(self, param, c, m1, r1):
    p,q,g,h,s = generate(param)
    #get a random message
    m2 = getRandomRange(1, q)
    r2 = ((m1 - m2 + s * r1) * inverse(s, q))%q
    return (m2, r2)


security = 80
m = 2

vv = verifier()
pp = prover()

param = vv.setup(security)

c, r = pp.commit(param, m)
print(vv.open(param, c, m, r))
m2, r2 = pp.fake_message(param, c, m, r)
print(vv.open(param, c, m2, r2))
