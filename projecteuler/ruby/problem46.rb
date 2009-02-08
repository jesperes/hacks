require 'eulerlib'


# Can n be written as the sum of a prime and twice a square
def follows_conjecture?(n)
  0.upto(n) do |i|
    break if i ** 2 > n
    2.each_prime do |p|
      num = p + 2 * (i ** 2)
      return true if num == n
      break if num > n
    end
  end
  return false
end

n = 5
while follows_conjecture?(n)
  n += 2
end
puts n

