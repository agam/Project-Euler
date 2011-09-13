#!/usr/bin/python

import getopt
import sys

def NumberOfDivisors(n):
    if n <= 1:
        return 1
    limit = n
    divisor = 1
    num_divisors = 0
    while divisor < limit:
        #print("Iterating ... limit = %d, divisor = %d, numdivs = %d" % (limit, divisor, num_divisors))
        if n % divisor == 0:
            limit = n / divisor
            #print("Found factor: %d" % divisor)
            num_divisors = num_divisors + 2
        divisor = divisor + 1
    return num_divisors

def TriangleNumber(n):
    """Returns the nth triangle number"""
    return n * (n + 1) / 2

def solve12():
    n = 1
    maxdivs = 1
    maxn = 1
    numdivs = 1
    while True:
        numdivs = NumberOfDivisors(TriangleNumber(n))
        print("Investigating %dth triangle number   -- %d [Maximum so far was %d with %d]" % (n, numdivs, maxn, maxdivs))
        if numdivs >= 500:
            break
        if numdivs > maxdivs:
            maxdivs = numdivs
            maxn = n
        n = n + 1
    print("Found %d with %d" % (TriangleNumber(n), numdivs))


euler_problems = {
        12: solve12,
        }

def main():
    optlist, args = getopt.getopt(sys.argv[1:], "hp:", ["help", "problem_number"])
    for o, a in optlist:
        if o == "-p":
           euler_problems[int(a)]()
        else:
            print("Usage: %s -p <problem-number>")
            sys.exit()

if __name__ == '__main__':
    main()

