def splitn(s, n)
  list = []
  0.upto(s.length / n) do |i|
    list << s[i*n..(i*n+n-1)]
  end
  return list
end

def is_cyclic_with_period(s, n, prefixlen)
  s0 = s[prefixlen..-1]
  cycle = s0[0..n-1]
  list = splitn(s0, n)
  list.each do |c|
    if c != cycle[0..(c.length)-1]
      return false
    end
  end

  return true
end


maxperiod = 0
maxd = 0

999.downto(2) do |n|
  s = ((10**1000)/n).to_s
  begin
    1.upto(1000) do |period|
      1.upto(10) do |prefix|
        if is_cyclic_with_period(s, period, prefix)
          if period > maxperiod
            puts "1/#{n} has period length #{period} (prefix #{prefix})"
            maxperiod = period
            maxd = n
          end
          raise
        end
      end
    end
  rescue
  end
end

puts maxperiod
puts maxd




