def alfavalue(s)
  val = 0
  s.each_byte do |letter|
    val += letter - 65 + 1
  end
  return val
end


File.open("names.txt") do |io|
  score = 0
  names = io.read.split(",").collect do |s| s.gsub(/"(.*)"/, '\1') end.sort
  names.each_index do |i|
    score += (i + 1) * alfavalue(names[i])
  end
  puts score
end


