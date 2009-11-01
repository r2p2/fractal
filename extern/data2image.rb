require 'rubygems'
require 'gd2'

include GD2

class Progress
  def initialize(min, max, current = 0)
    @min = min
    @max = max
    @current = current
    @old = "---"
  end

  def update(current)
    @current = current
  end

  def print_status_if_changed
    new = ((@current-@min).to_f/(@max-@min)*100).to_i
    if(@old != new)
      puts new.to_s + '%'
      @old = new
    end
  end
end

def convert_color(color)
  # from 0..255 : integer to 0..1 :float

  color.to_f/255
end

def extract_size_from_content(content)
  # width  = extract_size_from_content(content)[0]
  # height = extract_size_from_content(content)[1]
  # width, height : string

  content[0][0..-2].split(/ /)
end

def extract_color_from_line(line)
  # r = extract_color_from_line(line)[0]
  # g = extract_color_from_line(line)[1]
  # b = extract_color_from_line(line)[2]
  # r, g, b : string 

  line[0..-2].split(/ /)
end

def new_line?(x, width)
  x==width-1
end

def main(filename)
  data_file = File.open(filename, 'r')
  data_lines = data_file.readlines

  size = extract_size_from_content(data_lines)
  width  = size[0].to_i
  height = size[1].to_i
  x = 0
  y = 0
  
  progress = Progress.new(0, height)

  image = Image::TrueColor.new(width, height)

  image.draw do |pen|
    pen.thickness = 1
    
    data_lines[1..-1].each do |line|
      color = extract_color_from_line(line)
      pen.color = image.palette.resolve Color[convert_color(color[0]), convert_color(color[1]), convert_color(color[2])]
      pen.move_to x, y
      pen.line_to x, y

      if ! new_line?(x, width)
        x += 1
      else
        x  = 0
        y += 1
        progress.update(y)
        progress.print_status_if_changed
      end
    end

    image.export(filename+'.png')
  end
end
main(ARGV[0])
