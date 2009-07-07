penthash = {}

def pent(n)
  return n*(3*n - 1)/2
end

10000.times do |n|
  penthash[pent(n)] = n
end

#puts penthash.inspect
#exit 0

10000.times do |k0|
  k = k0 + 1
  pk = pent(k)

  puts pk
  k.times do |j0|
    j = j0 + 1
    pj = pent(j)
    next if pk == pj
    if penthash.has_key?(pk + pj) and penthash.has_key?(pk - pj)
      puts pk, pj, pk - pj
      exit 0
    end
  end
end
