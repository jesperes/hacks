def non_pandigital(s)
  return s =~ /(.).*\1/
end

# list is a list of integers. Return the list obtained by prepending
# 0..9 to each of the integers in list, and remove all resulting ints
# such that the first three integers form a number not divisble by the
# given prime and the entire integer does not form a pandigital
# number.
def filter(list, prime)
  newlist = []
  list.each do |n|
    # puts ">>>" + n.inspect
    (0..9).each do |i|
      all = i.to_s + n
      if non_pandigital(all)
        # puts "not pandigital: #{all}"
        next
      end

      if all[0..2].to_i % prime != 0
        # puts "#{all}: not divisible by #{prime}"
        next
      end

      newlist << all
    end
  end
  return newlist
end

def initial_list()
  list = []  
  1000.times do |n|
    next if n % 17 != 0
    s = sprintf("%03d", n)
    next if non_pandigital(s)
    list << s
  end
  return list
end

list = initial_list
primes = [1,2,3,5,7,11,13]
primes.reverse.each do |prime|
  list = filter(list, prime)
end
puts list.inject {|n, sum| sum.to_i + n.to_i}


