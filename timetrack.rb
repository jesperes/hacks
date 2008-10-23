require 'yaml'
require 'readline'
require 'tempfile'
require 'digest/sha1'

include FileTest

def from_tod(time)
  
end

# Return time of day as an integer
def get_tod(time)
  tod = nil
  
  if time.kind_of? Integer
    tod = time
  elsif time.kind_of? Time
    tod = sprintf("%02d%02d", time.hour, time.min).to_i
  else
    raise "Type error: #{time.inspect}"
  end
  
  # puts sprintf("tod(%s) = %d", time.to_s, tod)
  return tod
end

def format_duration(t)
  hh = (t / 3600).to_i
  mm = (t / 60).to_i
  
  return sprintf("%02d:%02d", hh, mm)
end

def short_date(time)
  return "" unless time
  time.strftime("%b %d %H:%M")
end


class WorkBlock
  attr_accessor :what, :from, :to, :key

  def initialize(what, from, to, key = nil)
    @what = what                # what I'm doing
    @from = from                # start time
    @to = to                    # end time
    @key = key ? key : Digest::SHA1.hexdigest(Time.now.to_f.to_s)
  end

  def duration
    if @to and @from
      return @to - @from
    else
      nil
    end
  end

  def to_s
    return "#{@what} #{short_date(@from)} => #{short_date(@to)}"
  end

  def to_yaml
    obj = {
      "key" => @key,
      "what" => @what,
      "to" => @to,
      "from" => @from,
    }
    return obj
  end

  # Stop working (now)
  def stop(time = Time.now)
    if @to
      puts "Workblock is not currently active."
    else
      @to = time
    end
  end

  def active
    @to == nil
  end
end

class WorkDBDaemon
  def initialize(workdb)
    @workdb = workdb
    @state = nil
    
    while true 
      now = Time.now
      state = get_state(now)
      if state != @state
        puts "State change to: #{state}"
        @state = state
      end 
      sleep(10)
    end
  end
  
  def get_state(time)
    if @workdb.is_during_lunch(time)
      :lunch
    elsif @workdb.is_during_work(time)
      :work
    else
      :free
    end
  end
end

class WorkDB
  @@WORKDBRC = File.join(ENV["HOME"], ".workdbrc")
  @@WORKDB = File.join(ENV["HOME"], ".workdb.yaml")

  def initialize
    @config = YAML::load_file(@@WORKDBRC)
    
    if File.exists?(@@WORKDB)
      @yaml_obj = YAML::load_file(@@WORKDB)
    else
      @yaml_obj = {
        "workblocks" => [],
        "current" => nil
      }
    end

    @current_date = @yaml_obj["date"]
    if not @current_date
      @current_date = Time.now
    end

    objs = @yaml_obj["workblocks"]
    @workblocks = []
    objs.each do |o|
      @workblocks << WorkBlock.new(o["what"],
                                   o["from"],
                                   o["to"],
                                   o["key"])
    end

    @wbmap = {}
    @workblocks.each do |wb|
      @wbmap[wb.key] = wb
    end
    
    @current_workblock = @wbmap[@yaml_obj["current"]]

    puts "Loaded #{@workblocks.length} records from #{@@WORKDB}"
    
    Thread.new(self) do |workdb|
      begin
        puts "Spawned deamon thread"
        WorkDBDaemon.new(workdb)
      rescue => msg
        puts "Exception in daemon thread: #{msg}"
        puts msg.backtrace.join("\n")
      end
    end

    at_exit { save }
  end
  
  def save
    puts "Saving #{@workblocks.length} records to #{@@WORKDB}"

    yaml_obj = { 
      "workblocks" => @workblocks.collect { |wb| wb.to_yaml },
      "current" => @current_workblock ? @current_workblock.key : nil,
      "date" => @current_date,
    }
    
    File.open(@@WORKDB, "w") do |io|
      YAML.dump(yaml_obj, io)
    end
  end

  def is_past_daybegin(time)
    if get_tod(get_daybegin) <= get_tod(time)
      return true
    else
      return false
    end
  end
  
  def is_past_dayend(time)
    if get_tod(time) > get_tod(get_dayend) 
      return true
    else
      return false
    end
  end

  def is_during_work(time)
    time = get_tod(time)

    if is_during_lunch(time)
      return false
    end

    if is_past_dayend(time)
      return false
    end

    if is_past_daybegin(time)
      return true
    end
  end
  
  def is_during_lunch(time)
    lunch_tod = get_lunch
    
    return if 
      get_tod(time) >= lunch_tod[0] and 
      get_tod(time) < lunch_tod[1]
  end
  
  def get_daybegin
    return @config["defaults"]["begin"].to_i
  end

  def get_dayend
    return @config["defaults"]["end"].to_i
  end

  def get_lunch
    @config["defaults"]["lunch"].collect do |s| s.to_i end
  end

  def parse_command(cmd)
    if not cmd or cmd.strip.length == 0
      return
    end
    
    args = cmd.split
    begin
      send(args.shift.intern, args)
    rescue ArgumentError => msg
      puts msg
      puts msg.backtrace.join("\n")
    rescue NoMethodError => msg
      puts msg
      puts msg.backtrace.join("\n")
    end
  end

  def help(args)
    puts "Usage:"
  end

  def quit(args)
    exit
  end

  def w(args)
    work(args)
  end

  def report(args)
    @workblocks.each do |wb|
      duration = wb.duration
      if duration
        puts "#{wb.what}: #{format_duration(wb.duration)} " + 
          "(#{short_date(wb.from)} => #{short_date(wb.to)})"
      else
        puts "#{wb.what}: active (#{short_date(wb.from)} =>"
      end
    end
  end
  
  def date(args)
    
  end

  def work(args)
    # Default: end current workblock, and start new one from now
    what = args.shift
    from = args.shift
    to = args.shift
    
    if @current_workblock != nil and @current_workblock.active
      @current_workblock.stop
    end

    if not what
      what = "unspecified"
    end

    from = Time.now if not from
    from = parse_time(from)
    to = parse_time(to)
    
    @current_workblock = WorkBlock.new(what ? what : "unspecified",
                                       from, to)
    @workblocks << @current_workblock
    
    if @current_workblock.active
      puts "Working on #{what} from #{short_date(@current_workblock.from)}"
    else
      puts "Added workblock:"
      puts @current_workblock
    end
  end

  def run
    while true
      begin
        parse_command(Readline.readline("command> "))
      rescue Interrupt
        if @current_workblock
          puts "Stopping current work block: #{@current_workblock}"
          @current_workblock.stop
        end
        raise
      end
    end
  end
end

if __FILE__ == $0
  workdb = WorkDB.new()

  begin
    workdb.run()
  rescue Interrupt
    puts "Interrupted."
  end
end