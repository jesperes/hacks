def seq(n)
  i = 0; return Array.new(n).collect do |n| i += 1 end
end

def sum(l)
  p = 1; l.each do |n| p += n end; return p
end

def sieve(max)
  list = [2] + seq(max).delete_if do |n| n <= 2 or n % 2 == 0 end
  p_index = 0

  while true
    prime = list[p_index]
    if prime >= Math.sqrt(max)
      break 
    end
    list = list.delete_if do |n| n > list[p_index] and n % prime == 0 end
    p_index += 1
  end
  return list
end

max = ARGV.shift.to_i
list = sieve(max)
puts sum(list)

