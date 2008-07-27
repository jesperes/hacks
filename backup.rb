#!/usr/bin/ruby

require 'fileutils'
require 'net/smtp'
require 'rmail'

include FileUtils
include FileTest

BACKUPDIR = "g:/"
SMTP_SERVERS = ["exchange.iar.se", "mailout.comhem.se"]
MAILFROM = "jesper@eskilson.se"
MAILTO = "jesper@eskilson.se"

def cygpath(p)
  IO.popen("cygpath -u \"#{p}\"") do |io|
    return io.readline.strip
  end
end

incrdir = Time.now.strftime("%Y%m%d-%H%M%S")
target = File.join(BACKUPDIR, "backup")
incrdir = cygpath(File.join(BACKUPDIR, "backup-incrementals", incrdir))
logfile = File.join(BACKUPDIR, "rsync.log")

rsync = "rsync"
rsync_opts = [
  "-rltoqi",
  "--delete",
  "--delete-excluded",
  "--modify-window=1",
  "--backup",
  "--backup-dir=#{incrdir}"
]

sources = [
  "C:/Documents and Settings/Jesper",
  "E:/"
]

exclude = [
  "NTUSER.DAT",
  "ntuser.dat.LOG",
  "UsrClass.dat",
  "UsrClass.dat.LOG",
  "Temporary Internet Files",
  "Thunderbird/**/parent.lock",
  "Mozilla/**/parent.lock",
  "Temp",
  "SecuROM",
  "Application Data/Google/Google Desktop",
  "Application Data/Last.fm/**/cache",
  "RECYCLER",
  "System Volume Information",
  "e/video",
  "e/Torrents",
  ".plugins/*/.history",
  "Mozilla/**/Cache",
  "Mozilla/**/Profiles/*/places.sqlite*",
  "*.gcf",                      # Steam/Valve game files, usually very large
  "*.bmc",                      # Terminal server client bitmap cache
  "Picasa2/**/*.db",            # Picasa thumbnails
  "ubuntu/disks/**",            # Ubuntu install is too large to backup
  logfile
]

mkdir_p target

messages = []

cmd = [rsync]
cmd.concat(rsync_opts)
cmd.concat(exclude.collect { |d| "--exclude=#{d}" })

sources.each do |src|
  cmd << cygpath(src)
end
cmd << cygpath(target)

cmdstring = cmd.collect { |arg| "\"#{arg}\"" }.join(" ")
cmdstring += " >#{logfile} 2>&1"

puts "Running backup script."
puts
puts "Backing up:"
sources.each { |d| puts "   #{d}" }
puts
puts "To: #{target}"

t0 = Time.now
puts "Backup started on: #{t0}"
status = system(cmdstring)
t0 = Time.now - t0
puts "Backup finished on: #{t0}"

if status
  File.open(File.join(BACKUPDIR, "last-successful-backup"), "w") do |io|
    io.puts("Last successful backup on #{Time.now} took #{t0} seconds.")
  end
else
  File.open(File.join(BACKUPDIR, "last-failed-backup"), "w") do |io|
    io.puts("Last failed backup (code #{status}) on #{Time.now} took #{t0} seconds.")
  end

  message = RMail::Message.new()
  message.header['To'] = MAILTO
  message.header['From'] = MAILFROM
  message.header['Subject'] = "Backup report (#{ENV['COMPUTERNAME']})"

  message.body = "Time taken: #{t0} seconds"
  message.body += "Status: #{status}\n"
  File.open(logfile) do |io|
    message.body += io.read
  end

  sent = false
  SMTP_SERVERS.each do |server|
    begin
      puts "Sending mail (using #{server})"
      Net::SMTP.start(server, 25) do |smtp|
        smtp.send_message(message.to_s, MAILFROM, MAILTO)
        sent = true
      end

      break
    rescue
      puts "Failed, trying next server."
      next
    end
  end

  if not sent
    puts "Failed to send mail report."
  end
end
