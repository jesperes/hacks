count = 0

1000.times do |exp|
  base = 1
  while true
    num_d = (base ** exp).to_s.length
    if num_d == exp
      puts "#{base} ** #{exp} == #{base ** exp} (#{num_d})"
      count += 1
    end
    if num_d > exp
      break
    end

    base += 1
  end
end

puts count
