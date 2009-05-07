#!/usr/bin/ruby

def gen_node(level, num_deps, node_name)

  if level <= 0
    puts
    puts "# Leaf node:"
    puts "#{node_name}:"
    puts "\ttouch $@"
    return
  end
  
  puts
  puts "# BEGIN: node_name = #{node_name}"

  num_deps.times { |i|
    dep_node_name = "#{node_name}-#{i}"
    puts "#{node_name}: #{dep_node_name}"
  }

  puts "#{node_name}:"
  puts "\ttouch $@"

  puts "# END: node_name = #{node_name}"
  puts

  num_deps.times { |i|
    dep_node_name = "#{node_name}-#{i}"
    gen_node(level - 1, num_deps, dep_node_name)
  }

end

s = "make_nodes/node"

puts "all: #{s}"
puts

gen_node(5, 5, s)

