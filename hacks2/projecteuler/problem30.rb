sum = 0
1000000.times do |n|
  next if n < 2
  powsum = 0
  n.to_s.each_char { |d| powsum += d.to_i ** 5 }
  if powsum == n
    puts n
    sum += powsum
  end
end

puts sum
