def decfrac
  n = 1
  i = 1
  while true
    n.to_s.each_byte do |b|
      yield(i, b - 48)
      i += 1
    end
    n += 1
  end
end

p = 1
decfrac do |i, n|
  if i == 1 or i == 10 or i == 100 or i == 1000 or i == 10000 or i == 100000 or i == 1000000
    puts "#{i} => #{n}"
    p *= n
  end
  if i == 1000000
    puts p
    break
  end
end

