# Returns true/false if the digit i is contained in number
def contains(number, width, i)
  
end

def gen234(d1s)
  (012..987).each do |d234|
    next if d234 % 2 == 1
    s = sprintf("%03d", d234)   # TODO possibly very slow
    next if s[d1s]              # skip all number with d1 in them
    yield d234
  end
end

def gen
  (1..9).each do |d1|
    gen234(d1.to_s) do |d234|
      
    end
  end
end
