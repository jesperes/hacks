$table = {}

def generate_triangles()
  sum = 1
  n = 1
  while true
    yield sum
    n += 1
    sum = sum + n
  end
end

def factors(n)
  [2, 5, 4, 6, 8, 10, 12, 36, 20, 50, 100].each do |t|
    if n % t != 0
      return 0
    end
  end

  orig_n = n
  #puts "Checking #{n}"
  numfactors = 0
  if n > 1
    $table.each_pair do |m, numf|
      if n % m == 0
        numfactors += numf
        #puts "#{n} % #{m} == 0, adding previously known factors for #{m}: #{numf}"
        n = n / m
        #puts "new n = #{n}"
      end
    end
  end
  
  # puts "Reduced n from #{orig_n} -> #{n}"
  
  n.times do |i|
    numfactors += 1 if n % (i+1) == 0
  end

  if orig_n > 1
    #puts "Remembering: #{orig_n} has #{numfactors} factors"
    $table[orig_n] = numfactors
  end
  
  return numfactors
end

max = 0
generate_triangles do |n|
  numfactors = factors(n)
  if numfactors > max
    puts "New max: #{n} has #{numfactors} factors"
    max = numfactors
  end

  if numfactors > 500
    puts n
    exit
  end
end
