require 'eulerlib'

# Can n be written as the sum of a prime and twice a square
def follows_conjecture?(n)
  puts "Checking #{n}"
  1.upto(n) do |i|
    p = 2
    while p <= n
      num = p + 2 * (i ** 2)

      return true if num == n

      p = get_next_prime(p)
    end
  end
  return false
end

n = 5
while follows_conjecture?(n)
  n += 2
end
puts n

