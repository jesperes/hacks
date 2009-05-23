sum = 0
1_000_000.times do |n|
  b10 = n.to_s
  b2 = sprintf("%b", n)
  if b10 == b10.reverse and b2 == b2.reverse
    sum += n
  end
end
puts sum
