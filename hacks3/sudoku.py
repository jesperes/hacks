#!/usr/bin/python

import types
import sys

emptyset = set()

class EmptyDomain(Exception):
    pass

class UndefinedValue(Exception):
    pass

class ViolatedConstraint(Exception):
    pass


class NotEqualConstraint:
    def __init__(self, x0, x1):
        self.x0 = x0
        self.x1 = x1

    def propagate(self):
        if self.x0.isbound():
            if self.x1.isbound():
                # both x0 and x1 are bound, just check that they are
                # different
                
                if self.x0.value() == self.x1.value():
                    raise ViolatedConstraint()

            else:
                # x0 is bound and x1 is not; subtract x0 from x1's domain
                self.x1.subtract_value(self.x0.value())
        else:
            if self.x1.isbound():
                # x0 is not bound, but x1 is -- substract x1 from x0's domain
                self.x0.subtract_value(self.x1.value())
            else:
                # none of them are bound, do nothing
                pass
            
                
                

class V:
    def __init__(self, domain):
        if type(domain) == type(emptyset):
            self.domain = domain
        else:
            self.domain = set([domain])

        self.constraints = []

        
    def __str__(self):
        return str(self.domain)

    def __repr__(self):
        return str(self.domain)

    def isbound(self):
        return len(self.domain) == 1

    def value(self):
        if self.isbound():
            return tuple(self.domain)[0]
        else:
            raise UndefinedValue()

    def propagate(self):
        for c in self.constraints:
            c.propagate()

    def subtract_value(self, v):
        if v not in self.domain:
            return
    
        if len(self.domain) == 1:
            raise EmptyDomain()
        else:
            self.domain.remove(v)
            if len(self.domain) == 0:
                raise EmptyDomain()
            
            
    def not_equal_to(self, var):
        self.constraints.append(NotEqualConstraint(self, var))
    
        
    
# sudoku = [ [ V(),  V(), V(4),   V(),  V(3), V(),  V(),  V(),  V()  ],
#            [ V(6), V(), V(),    V(9), V(1), V(2), V(3), V(),  V()  ],
#            [ V(3), V(), V(),    V(),  V(),  V(),  V(),  V(7), V()  ],
           
#            [ V(),  V(),  V(),   V(),  V(), V(3),  V(8), V(),  V(9) ],
#            [ V(),  V(7), V(),   V(),  V(), V(),   V(),  V(4), V()  ],
#            [ V(2), V(),  V(3),  V(6), V(), V(),   V(),  V(),  V()  ],
           
#            [ V(), V(3), V(),    V(),  V(),  V(),  V(),  V(),  V(5) ],
#            [ V(), V(),  V(2),   V(4), V(8), V(9), V(),  V(),  V(1) ],
#            [ V(), V(),  V(),    V(),  V(2), V(),  V(7), V(),  V()  ] ]


s = [ V(domain = set([1,2,3])), V(1), V(2) ]

s[0].not_equal_to(s[1])
s[0].not_equal_to(s[2])
s[1].not_equal_to(s[2])

def enumerate(vars):
    for v in vars:
        v.propagate()
        print v
    
enumerate(s)

