#!/usr/bin/ruby

require 'fileutils'
require 'net/smtp'

include FileUtils
include FileTest

BACKUPDIR = "D:/"
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
  "-n",
  "-vrultoi",
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
  ".plugins/*/.history",
  "Mozilla/**/Cache",
  "Mozilla/**/Profiles/*/places.sqlite*",
  "*.gcf",                      # Steam/Valve game files, usually very large
  "*.bmc",                      # Terminal server client bitmap cache
  "Picasa2/**/*.db",            # Picasa thumbnails
  "**/video/ISO",               # Don't backup DVD ISO images
  cygpath(logfile)
]

mkdir_p target

messages = []

cmd = []
cmd += rsync_opts
cmd += exclude.collect { |d| "--exclude=#{d}" }
cmd += sources.collect { |src| cygpath(src) }
cmd << cygpath(target)

message = ""
message += "To: #{MAILTO}\n"
message += "From: #{MAILFROM}\n"
message += "Subject: Backup report (#{ENV['COMPUTERNAME']})\n"
message += "\n"

cmdstring = cmd.collect { |arg| "\"#{arg}\"" }.join(" ")

t0 = Time.now
IO.popen("rsync " + cmdstring).readlines do |line|
  message += line
  puts message
end
t0 = Time.now - t0

message += "Time taken: #{t0} seconds\n"
status = true

if status
  File.open(File.join(BACKUPDIR, "last-successful-backup"), "w") do |io|
    io.puts("Last successful backup on #{Time.now} took #{t0} seconds.")
  end
else
  File.open(File.join(BACKUPDIR, "last-failed-backup"), "w") do |io|
    io.puts("Last failed backup (code #{status}) on #{Time.now} took #{t0} seconds.")
  end
end


if true # not status
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
