def non_pandigital(s)
  return s =~ /(.).*\1/
end

# list is a list of integers. Return the list obtained by prepending
# 0..9 to each of the integers in list, and remove all resulting ints
# such that the first three integers form a number not divisble by the
# given prime and the entire integer does not form a pandigital
# number.
def prepend(list, prime)
  newlist = []
  lists.each do |n|
    (0..9).each do |i|
      all = i.to_s + n.to_s
      next if non_pandigital(all)
      next unless all[0..2].to_i % prime == 0
      newlist << all
    end
  end
  return newlist
end

