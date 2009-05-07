require 'sieve'

primes = sieve(10000)
pmap = {}
primes.each do |p|
  pmap[p] = true
end

def is_perm(a, b)
  as = []
  a.to_s.each_char do |c|
    as << c
  end

  bs = []
  b.to_s.each_char do |c|
    bs << c
  end

  as.sort!
  bs.sort!

  return as == bs
end



primes.each do |p0|
  1.upto(9999) do |step|
    p1 = p0 + step
    p2 = p1 + step
    break if p2 > primes.last
    if pmap.has_key?(p1) and pmap.has_key?(p2) and is_perm(p0, p1) and is_perm(p1, p2)
      puts "p0 = #{p0}, p1 = #{p1}, p2 = #{p2}, step == #{step}"
      puts p0.to_s + p1.to_s + p2.to_s
    end
  end
end
