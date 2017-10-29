require 'eulerlib'

# Writing integers as the sum of two abundant numbers.
#
# The largest number that cannot be expressed as a sum of two abundant
# numbers is less than 28123.

abundant_nums = []

4000.times do |n|
  if abundant(n)
    if n % 1000 == 0
      puts n
    end
    abundant_nums << n
  end
end

