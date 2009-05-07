BIGNUM = 600851475143 
limit = Math.sqrt(BIGNUM)

m = BIGNUM
n = 1
while n < limit
  if m % n == 0
    m = m / n
    puts n
  end
  n += 1
end
