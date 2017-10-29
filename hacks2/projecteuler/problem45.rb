def triangle_number(from, limit)
  list = []
  from.upto(limit) do |n|
    list << n * (n + 1)/2
  end
  return list
end

def pentagonal_number(from, limit)
  list = []
  from.upto(limit) do |n|
    list << n * (3*n - 1)/2
  end
  return list
end

def hexagonal_number(from, limit)
  list = []
  from.upto(limit) do |n|
    list << n * (2*n - 1)
  end
  return list
end

LIMIT = 10000
tlist = triangle_number(286, LIMIT)
plist = pentagonal_number(166, LIMIT)
hlist = hexagonal_number(144, LIMIT)


hlist.each do |h|
  plist.each do |p|
    break if p > h
    if p == h
      tlist.each do |t|
        break if t > p
        if t == p
          puts t
          exit 0
        end
      end
    end
  end
end
