require 'eulerlib'

primes = sieve(1000000)

dsq = {}

# return true if n can be written as 2 * m^2
def is_double_sq(n)
  sq = n/2
  rt = Math.sqrt(sq)
  is_dsq = (sq == (rt * rt))
  if is_dsq
    dsq[
end

n = 3
while true
  # puts n
  found = false
  primes.each do |p|
    break if p > n
    sq = (n - p)
    if is_double_sq(sq)
      puts "#{n} == #{p} + #{n-p}^2"
      found = true
      break
    end
  end

  if not found
    puts n
    break
  end

  n += 2
end
