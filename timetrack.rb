require 'yaml'
require 'readline'
require 'tempfile'
require 'digest/sha1'

include FileTest

def make_time(date, tod)
  if tod.kind_of? Time
    hh = tod.hour
    mm = tod.min
  else
    tod = tod.to_i
    hh = (tod / 100).to_i
    mm = (tod % 100)
  end
  
  array = date.to_a
  array[2] = hh
  array[1] = mm
  array[0] = 0
  t = Time.local(*array)
  return t
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
  
  return tod
end

def format_duration(t)
  hh = (t / 3600).to_i
  t = t - hh * 3600
  mm = (t / 60).to_i
  
  return sprintf("%02d:%02d", hh, mm)
end

def short_date(time)
  return "" unless time
  time.strftime("%Y-%m-%d %H:%M")
end


class WorkBlock
  attr_accessor :what, :from, :to, :key

  def initialize(what, from, to, key = nil)
    @what = what                # what I'm doing
    @from = from                # start time
    @to = to                    # end time
    @key = key ? key : Digest::SHA1.hexdigest(Time.now.to_f.to_s)
  end

  # Returns the duration in hours
  def duration
    if @to and @from
      return (@to - @from)/3600.0
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
      puts "Ending workblock #{@what} at #{short_date(@to)}"
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

    puts "Current date: #{@current_date.strftime("%Y-%m-%d")}"
    puts "Day length: #{hour_decimal_to_hours_minutes(normal_daylength)}"
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

  def normal_daylength
    lunch = get_lunch
    lunch_length = (make_time(@current_date, lunch[1]) - 
                    make_time(@current_date, lunch[0]))

    start = make_time(@current_date, get_daybegin)
    stop = make_time(@current_date, get_dayend)
    return (stop - start - lunch_length)/3600
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

  def is_work(what)
    @config["notwork"].each do |notwork|
      if what == notwork
        return false
      end
    end

    return true
  end

  def same_day(t0, t1)
    return t0.strftime("%x") == t1.strftime("%x")
  end

  def help(args = [])
    puts "Usage:"
  end

  def quit(args = [])
    exit
  end

  def stop(args = [])
    if @current_workblock != nil and @current_workblock.active
      @current_workblock.stop
    end
  end

  def w(args = [])
    work(args)
  end

  def hour_decimal_to_hours_minutes(hours)
    hh = hours.to_i
    mm = (hours - hh) * 60.0
    return sprintf("%d:%02d", hh, mm)
  end

  def worked_today()
    hours = 0.0
    @workblocks.each do |wb|
      next unless same_day(wb.from, @current_date)
      if is_work(wb.what)
        hours += wb.duration
      end
    end
    return hours
  end

  def report(args = [])
    type = args.shift
    if not type
      type == "full"
    else
      type = type.strip
    end
    
    if type == "daily"
      whatmap = {}
      whatmap.default = []
      
      puts "Report for #{@current_date}"

      daybegin = 9999
      dayend = 0
      hours_worked = 0.0

      # Print a summary for the current day
      @workblocks.each do |wb|
        next unless same_day(wb.from, @current_date)
        whatmap[wb.what] = whatmap[wb.what] + [wb]
      end

      whatmap.keys.sort.each do |what|
        puts "#{what}"
        blocks = whatmap[what].each do |wb|
          from = get_tod(wb.from)
          to = get_tod(wb.to)

          puts "   #{from} => #{to}"
          daybegin = [from, daybegin].min
          dayend = [to, dayend].max

          if is_work(what)
            hours_worked += wb.duration
          end
        end
      end
      
      puts "Total day: #{daybegin} => #{dayend}"
      puts "Total hours worked: #{hour_decimal_to_hours_minutes(hours_worked)}"
    else
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
  end
  
  def date(args = [])
    s = args.shift
    Tempfile.open("workdb") do |io|
      io.close
      system("touch --date=\"#{s}\" #{io.path}")
      @current_date = File.stat(io.path).mtime
    end
    
    puts "Current date: #{@current_date.strftime("%Y-%m-%d")}"
  end
  
  def work(args = [])
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
    
    if from
      from = make_time(@current_date, from)
    else
      from = make_time(@current_date, Time.now)
    end
    
    if to != nil
      to = make_time(@current_date, to)
    end
    
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
        date = @current_date.strftime("%Y-%m-%d")
        time_left_today = normal_daylength - worked_today
        prompt = sprintf("%s (worked %s, %s left)> ",
                         date,
                         hour_decimal_to_hours_minutes(worked_today),
                         hour_decimal_to_hours_minutes(time_left_today))
        parse_command(Readline.readline(prompt))
      rescue Interrupt
        stop
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
