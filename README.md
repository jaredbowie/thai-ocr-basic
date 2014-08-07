# thaiocr

Read Thai bmps extracted from a dvd to text and put them into an associated subtitle file.<br>
This version uses 2-pass character extraction, but will not guess what characters are will only go on exact matches, which is for the most part what subtitles are.  

## Usage

requires mongodb<br>
Rip a dvd.<br>
Use subrip to extract the bmps and produce a srt subtitle file.  <br>
Set the parameters in the -main of character-reading.clj<br>
Run and type characters as they appear.<br>
Done.<br>

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
