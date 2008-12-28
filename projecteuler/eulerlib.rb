def sum_divisors(n)
  sum = 0
  (n-1).times do |i|
    j = i + 1
    sum += j if n % j == 0
  end
  return sum
end

def divisors(n)
  divs = []
  (n-1).times do |i|
    j = i + 1
    divs << j if n % j == 0
  end
  return divs
end


def abundant(n)
  sum = 0
  (n-1).times do |i|
    j = i + 1
    sum += j if n % j == 0
    if sum > n
      return true
    end
  end
  if sum > n
    return true
  end
end

def deficient(n)
  sum = 0
  (n-1).times do |i|
    j = i + 1
    sum += j if n % j == 0
    if sum < n
      return true
    end
  end
  if sum < n
    return true
  end
end

def perfect(n)
  sum_divisors(n) == n
end

def num_distinct_prime_divisors(n)
  numdivs = 0
  p = 2
  while p <= n
    if n % p == 0
      numdivs += 1
      while n % p == 0
        n = n / p
      end
    else
      p += 1
    end
  end

  return numdivs
end

def problem47
  n = 1
  npf = 4
  cons = []

  while true
    n += 1

    num_factors = num_distinct_prime_divisors(n)
    if num_factors >= npf
      cons << n
      if cons.length >= npf
        return cons
      end
    else
      cons = []
    end
  end
end

def sieve(limit)
  prime_list = []
  primes = (2..limit).to_a
  nexti = 0
  while true
    while nexti < limit
      p = primes[nexti]
      break if p != nil
      nexti += 1
    end

    return prime_list if nexti >= limit

    prime_list << p
    i = nexti
    while i < limit
      primes[i] = nil
      i += p
    end
  end
  raise
end

if __FILE__ == $0
  # puts perfect(ARGV.shift.to_i)
  # puts num_distinct_prime_divisors(ARGV.shift.to_i, {})
  # puts problem47().inspect
  primes = sieve(ARGV.shift.to_i)
  puts primes.length
end
