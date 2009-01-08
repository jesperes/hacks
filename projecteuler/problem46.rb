require 'eulerlib'
limit = ARGV.shift.to_i

puts "Sieving..."
primes = sieve(limit)
puts "Primes: #{primes.length}"

puts "Calculating double squares..."
dsqlist = []
n = 0
begin
  dsq = 2 * n * n
  dsqlist[n] = dsq
  n += 1
end while dsq < limit
puts "Double squares: #{dsqlist.length}"

max = 0
primes.each do |p|
  numbers = Array.new(p - max)
  puts "Checking numbers [#{max}, #{p}]"
  max.upto(p) do |n|
    dsqlist.each do |dsq|
      m = p + dsq
      if m > max and m < p
        numbers[n - max] = true
      else
        break
      end
    end

    if numbers.find { |n| n != true }
      puts n
      exit 0
    end

    max = p - 1
  end
end

puts "No number found."
