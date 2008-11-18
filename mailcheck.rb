#!/usr/bin/ruby

current = []

while true
  newmail = []
  IO.popen("mailcheck -b").readlines.each do |line|
    if line =~ /(\d+) new/
      newmail << line
    end
  end
  
  if newmail != current and newmail.length > 0
    puts "\r" + "-" * 60
    current = newmail
    current.each do |s|
      puts "\r#{s}"
    end
   end
  
  sleep(5)
end
