require 'fac'

sum = 0
n = 3
while true 
  facsum = 0
  n.to_s.each_char do |d| facsum += fac(d.to_i) end
  if n % 100000 == 0
    puts "Checking #{n}"
  end
  if facsum == n
    sum += n
    puts "#{n}, sum = #{sum}"
  end
  n += 1
end
