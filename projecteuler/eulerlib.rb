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

if __FILE__ == $0
  puts perfect(ARGV.shift.to_i)
end
