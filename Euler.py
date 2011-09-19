#!/usr/bin/python

import getopt
import numpy as np
import re
import sys

def NumberOfDivisors(n):
    """Return the number of divisors for a given number"""
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
    """Find the first triangle number to have more than 500 divisors"""
    n = 1
    maxdivs = 1
    maxn = 1
    numdivs = 1
    while True:
        numdivs = NumberOfDivisors(TriangleNumber(n))
        #print("Investigating %dth triangle number   -- %d [Maximum so far was %d with %d]" % (n, numdivs, maxn, maxdivs))
        if numdivs >= 500:
            break
        if numdivs > maxdivs:
            maxdivs = numdivs
            maxn = n
        n = n + 1
    print("Found %d with %d" % (TriangleNumber(n), numdivs))

def solve13():
    """Find the first 10 digits of the sum of one hundred 50-digit numbers.
       Data is stored in data/problem13.
       [Use the fact that python automatically handles large numbers --
       see http://www.python.org/dev/peps/pep-0237/]"""
    sum = 0
    f = open('data/problem13', 'r')
    for line in f:
        val = int(line.strip())
        sum += val
    print("sum is %d" % sum)

def get_hotpo_length(num, memoized_lengths):
    if num == 2:
        return 2  # 2->1
    if num < 100000 and memoized_lengths[num] > 0:
        #print("Found precalculated length %d for %d" % (memoized_lengths[num], num))
        return memoized_lengths[num]
    if num % 2 == 0:
        return 1 + get_hotpo_length(num/2, memoized_lengths)
    else:
        return 1 + get_hotpo_length(3 * num + 1, memoized_lengths)

def solve14():
    """Find the number less than a million which produces the longes chain for the HalfOrTriplePlusOne chain"""
    longest_sequence = (1, 1)
    # Memoize the first 100,000 numbers
    memo_limit = 100000
    memoized_lengths = np.zeros(100000)
    for num in xrange(2, 1000000):
        length = get_hotpo_length(num, memoized_lengths)
        #print("Length is %d" % length)
        if num < 100000:
            memoized_lengths[num] = length
        if (length, num) > longest_sequence:
            longest_sequence = (length, num)
            #print("Longest sequence is (%d, %d)" % (longest_sequence[0], longest_sequence[1]))

    print("Finally, longest sequence was : %d for %d" %
            (longest_sequence[0], longest_sequence[1]))

def factorial(num):
    if num == 1:
        return 1
    else:
        return num * factorial(num-1)

def solve15():
    """Find the number of ways to traverse a 20x20 grid across a diagonal"""
    # 2N C N is the number of ways to select a path across the diagonal (here N = 20)

    result = factorial(40) / factorial(20) / factorial(20)
    print("The answer is: %d" % result)

def sum_of_digits(num):
    sum = 0
    while num > 0:
        sum += (num % 10)
        num = num / 10
    return sum

def solve16():
    """Find the sum of  the digits in 2 ^ 1000"""
    two_to_thousand = 2**1000
    result = sum_of_digits(two_to_thousand)
    print("The answer is: %d" % result)

def solve17():
    """Find the number of words used in writing out all numbers from 1 to 1000"""
    one_len = len("one")
    digits = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
    tens = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"] # all except ten
    teens = ["", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

    all_numbers = []
    # Start with the single digits
    for hundred in range(10):
        hundred_prefix = ""
        if hundred > 0:
            all_numbers.append(digits[hundred] + " hundred ")
            hundred_prefix = digits[hundred] + " hundred and "
        for ten in range(10):
            if ten > 0:
                all_numbers.append(hundred_prefix + tens[ten])
            ten_prefix = ""
            if ten > 1:
                ten_prefix = tens[ten] + " "
            for digit in range(1, 10):
                if ten == 0:
                    all_numbers.append(hundred_prefix + digits[digit])
                elif ten == 1:
                    all_numbers.append(hundred_prefix + teens[digit])
                else:
                    all_numbers.append(hundred_prefix + ten_prefix + digits[digit])
    all_numbers.append("one thousand")

    # Print them all out, while adding them up
    result = 0
    for number in all_numbers:
        # print(number)     ## Uncomment this for debug
        tempstring = re.sub(r' ', '', number)
        result = result + len(tempstring)
    print("\n\nThe answer is: %d" % result)


euler_problems = {
        12: solve12,
        13: solve13,
        14: solve14,
        15: solve15,
        16: solve16,
        17: solve17,
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

