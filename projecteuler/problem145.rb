

def reversible(i)
  return false if i % 10 == 0
  irev = i.to_s.reverse
  n = i + irev.to_i
  n.to_s.each_byte do |b|
    return false if b % 2 == 0
  end
  return true
end

num_reversible = 0
1.upto(1000000) do |i|
  num_reversible += 1 if reversible(i)
end

puts num_reversible
