def seq(n)
  i = 0
  return Array.new(n).collect do |n| i += 1 end
end

def get_nth_prime(nth, max = 100)
  num_primes_found = 0
  
  while true
    list = [2] + seq(max).delete_if do |n| n <= 2 or n % 2 == 0 end
    p_index = 0
    puts "max = #{max}"

    while true
      puts "list = #{list.inspect}"
      prime = list[p_index]
      num_primes_found += 1
      if num_primes_found >= nth
        return prime
      end
      if prime >= Math.sqrt(max)
        puts "max prime = #{prime}"
        break 
      end
      list = list.delete_if do |n| n > list[p_index] and n % prime == 0 end
      p_index += 1
    end
    max *= 2
  end
end

n = ARGV.shift.to_i
t0 = Time.now
nth_prime = get_nth_prime(n)
t0 = Time.now - t0
puts "The #{n}th prime is #{nth_prime} (#{t0} seconds)"
