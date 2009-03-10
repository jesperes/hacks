def read_prompt(prompt, default = nil)
  s = nil
  while true
    $stdout.print prompt
    if default
      $stdout.print " (default \"#{default}\")"
    end

    $stdout.print ": "
    $stdout.flush
    s = $stdin.gets.strip
    if s == ""
      return default
    end
    return s if s != ""
  end
end

def get_templates()
  return Dir[File.join(File.dirname(__FILE__), "*.template")]
end

def print_templates()
  puts "Available templates: "
  get_templates.each do |f|
    puts "  #{File.basename(f, ".template")}"
  end
end

print_templates

if ARGV.length > 0
  default_template = ARGV.shift
else
  default_template = File.basename(get_templates[0], ".template")
end
template = read_prompt("Template name", default_template)

template_file = get_templates().find { |t| File.basename(t) =~ /^#{template}.*/ }
puts "Using template: #{File.basename(template_file)}"

source_dir = read_prompt("Source dir", Dir.pwd)
classname = read_prompt("Classname")
file_prefix = ""
classname_noprefix = classname

if classname =~ /([A-Z][a-z]+)([A-Z][a-z]+)/
  file_prefix = $1
  classname_noprefix = $2
else
  puts "Failed to get file_prefix from classname \"#{classname}\""
end

vars = {
  "FILE_PREFIX" => file_prefix,
  "FILE_PREFIX_UPPER" => file_prefix.upcase,
  "CLASSNAME" => classname,
  "CLASSNAME_UPPER" => classname.upcase,
}

def replace_vars(s, vars)
  vars.each_pair do |var, value|
    s.gsub!(/\$\{#{var}\}/, value)
  end
  return s
end


f = nil
IO.readlines(template_file).each do |line|
  if line =~ /^---(.*)=(.*)---/
    name = replace_vars($1.strip, vars)
    file = replace_vars($2.strip, vars)
    f.close if f
    f = File.new(File.join(source_dir, file), "w")
    puts "Writing #{f.path}"
    next
  elsif line =~ /^=== query=(.*),(.*)===/
    var = $1.strip
    prompt = $2.strip
    value = read_prompt(prompt)
    vars[var] = value
    vars[var + "_UPPER"] = value.upcase
    next
  end

  f.print replace_vars(line, vars)
end

f.close if f
puts "Done."

