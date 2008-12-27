limit = ARGV.shift.to_i
n = 0
dsq = {}
begin 
  n += 1
  dsq[n] = 2 * n * n
end while dsq[n] < limit
  

puts dsq

