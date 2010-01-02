key = "aaa"

while true
  puts "key = #{key}"
  keylist = []
  key.each_byte do |c| keylist << c end

  cleartext = []
  File.open("../files/cipher1.txt") do |io|
    bytes = io.read().strip().split(",").collect do |b| b.to_i end
    n = bytes.length
    n.times do |i|
      cleartext << (bytes[i] ^ keylist[i % 3])
    end

    s = cleartext.collect do |b| "%c" % b end.join
    if s =~ /^[a-zA-Z ]+$/
      puts s
      exit 0
    end
  end

  key = key.succ
end
