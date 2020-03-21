from fractions import gcd
import random

class Prover():
    def __init__(self, tc, s):
        assert gcd (s, tc.n) == 1, 's and n are not co-prime'
        self.n = tc.n
        self.__s = s
        tc.set_up((self.__s ** 2) % self.n)


    def generate(self):
        self.r = random.randint(1, self.n - 1)
        x = (self.r ** 2) % self.n
        return x

    def response(self, e):
        y = (self.r * self.__s) % self.n if e else self.r
        return y




class Verifier():
    def __init__(self, tc):
        self.tc = tc


    def set_up(self, x):
        self.x = x


    def challenge(self):
        self.e = random.randint(0, 1)
        return self.e

    def verify(self, y):
        test = (self.x * self.tc.v ** self.e) %self.tc.n
        if y == 0 or (y**2)%self.tc.n != test:
            return False
        return True


class TrustedCenter():
    def __init__(self, n):
        self.n = n

    def set_up(self, v):
        self.v = v



def fiat_shamir_initialization(n, s):
    tc = TrustedCenter(n)
    peggy = Prover(tc, s)
    return tc, peggy


def fiat_shamir_identification(t, tc, peggy):
    victor = Verifier(tc)
    for _ in xrange(t):
        victor.set_up(peggy.generate())
        c = victor.verify(peggy.response(victor.challenge()))
        if not c:
            print 'Reject'
            return
    print 'Accept'



tc, peggy = fiat_shamir_initialization(35, 16)
fiat_shamir_identification(10, tc, peggy)
