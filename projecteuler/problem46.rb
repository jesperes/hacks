require 'eulerlib'
limit = ARGV.shift.to_i

puts "Sieving..."
primes = sieve(limit)
puts "Primes: #{primes.length}"

puts "Calculating double squares..."
dsqlist = []
n = 1
begin
  dsq = 2 * n * n
  dsqlist[n] = dsq
  n += 1
end while dsq < limit
puts "Double squares: #{dsqlist.length}"

max = 0
primes.each do |p|
  numbers = Array.new(p - max)
  max.upto(p) do |n|
    dsqlist.each do |dsq|
      m = p + dsq
      if m > max and m <
      numbers[n - max]
    end
  end
end

