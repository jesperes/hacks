# Euler problem 28. You could actually calculate the sum, but this one
# actually constructs the spiral as well!

class Spiral
  attr_reader :spiral

  def initialize(size)
    @dx = 0
    @dy = -1
    @i = 1

    if size % 2 == 0
      @size = size + 1
      @origo = size/2
    else
      @origo = size/2
      @size = size
    end

    @x = @y = @origo

    @spiral = Array.new(@size) do |i|
      Array.new(@size) { 0 }
    end
  end
  
  def fill
    limit = @size ** 2
    begin
      @spiral[@y][@x] = @i
      @i += 1

      if @dx == 0 and @dy == 1
        dx = -1
        dy = 0
      elsif @dx == 0 and @dy == -1
        dx = 1
        dy = 0
      elsif @dx == 1 and @dy == 0
        dx = 0
        dy = 1
      elsif @dx == -1 and @dy == 0
        dx = 0 
        dy = -1
      end
      
      if (dx + dy).abs != 1
        raise
      end
      
      # if turning right yields an empty square, do it
      if @spiral[@y + dy][@x + dx] == 0
        @dx = dx
        @dy = dy
      end
      
      @x += @dx
      @y += @dy
    end while @i <= limit
  end

  def to_s
    s = ""
    @spiral.each do |row|
      s += (row.collect do |n| 
        if n == 0
          "..."
        else
          sprintf("%3d", n)
        end
      end.join(" "))
      s += "\n"
    end

    return s
  end

  def sum
    s = 0
    @spiral.each_index do |y|
      @spiral[y].each_index do |x|
        offx = x - @origo
        offy = y - @origo
        
        if offx.abs == offy.abs
          n = @spiral[x][y]
          # puts "On diagonal: #{n}"
          s += n
        end
      end
    end
    
    return s
  end
end

spiral = Spiral.new(ARGV.shift.to_i)
spiral.fill
# puts spiral
puts spiral.sum

