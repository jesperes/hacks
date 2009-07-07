def permute0(list, rest, &block)
  rest.each do |n|
    restrest = rest.collect { |x| x if x != n }.compact
    if restrest.length == 0
      block.call(list + [n])
    else
      permute0(list + [n], restrest, &block)
    end
  end
end

def permute(list, &block)
  permute0([], list, &block)
end

n = 1
limit = 1000000
permute([0,1,2,3,4,5,6,7,8,9]) do |list|
  if n == limit
    puts list.join("")
    break
  end
  n += 1
end
