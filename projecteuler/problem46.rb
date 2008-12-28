require 'eulerlib'
limit = ARGV.shift.to_i
puts "Sieving..."
primes = sieve(limit)

max = 0

