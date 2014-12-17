from numpy import *

def sa(s):
    """Apply Burrows-Wheeler transform to input string."""
    assert "$" not in s, "Input string cannot contain null character ('\\0')"
    s += "$"  # Add end of file marker
    otable=[]
    for i in range(len(s)):
        otable.append(s[i:] + s[:i])
    table = sorted(s[i:] + s[:i] for i in range(len(s)))  # Table of rotations of string
    sa=list(otable.index(v) for v in table)
    return sa  # Convert list of characters into string

def invsa(s):
    invs=zeros(len(s))
    for i in range(len(s)):
        invs[s[i]]=i
    return invs
    
def lcp(t, sa, invsa):
    l=0
    lcp=zeros(len(sa)-1)
    for i in range(len(sa)):
        k=invsa[i]
        j=sa[k-1]
        while (t[i+l]==t[j+l]):
            l=l+1
        lcp[k]=l
        if l>0:
            l=l-1
    return lcp
   
t= "banana"      
sa= sa(t)
print sa
invsa=invsa(sa)
print invsa
lcp=lcp(t,sa,invsa)
print lcp

