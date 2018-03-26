# Prime sieve implementation in ruby


def sieve(limit)
  primes = [2, 3]
  sieve = (0..limit).collect do |n|
    if n < 3 or n % 2 == 0 or n % 3 == 0
      :notprime
    else
      n
    end
  end

  prime = 0
  while true
    prime += 1

    while sieve[prime] == :notprime
      prime += 1
      if prime > limit
        return primes
      end
    end

    primes << prime
    
    i = prime*2
    while i <= sieve.length
      sieve[i] = :notprime
      i += prime
    end
  end
end

if __FILE__ == $0
  primes = sieve(ARGV.shift.to_i)
  puts primes.length
end
