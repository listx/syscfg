#!/usr/bin/ruby
# Author: Linus Arver (C) 2009
# License: GPL3
# Program Name: ureader.rb
# Description: extensions recognized

class Ext
    @doc = %w{doc odf odt rtf}
    @web = %w{htm html}
    @pdf = %w{pdf ps}
    @img = %w{bmp gif jpg jpeg png svg tiff}
    @img2 = %w{psd xcf}
    @audio = %w{flac mp3 wma}
    @midi = %w{mid midi}
    @movie = %w{avi flv ogg mkv mov mp4 mpg mpeg wmv}

    class<<self; attr_reader :doc end
    class<<self; attr_reader :web end
    class<<self; attr_reader :pdf end
    class<<self; attr_reader :img end
    class<<self; attr_reader :img2 end
    class<<self; attr_reader :audio end
    class<<self; attr_reader :midi  end
    class<<self; attr_reader :movie end
end
