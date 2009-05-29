def cancel(nom, denom)
  1.upto(9) do |digit|
    if nom.to_s.include?(digit.to_s) and denom.to_s.include/(digit.to_s)
      
    end
  end
end

10.upto(99) do |nom|
  nom_s = nom.to_s
  10.upto(99) do |denom|
    denom_s = denom.to_s
    1.upto(9) do |digit|
      ds = digit.to_s

      if nom_s.include?(ds) and
          denom_s.include?(ds)
        
        n = nom_s.delete(ds).to_f
        d = denom_s.delete(ds).to_f
        if d > 0 and n != d and n/d == nom/denom
          puts "#{nom}/#{denom} == #{n}/#{d}"
        end
      end
    end
  end
end
