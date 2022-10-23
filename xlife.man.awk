BEGIN {
   c = sprintf("%c", 27) "["
   black = c "32m"
   yellow = c "33m"
   italic = c "3m"
   white = c "31m"
   bold = c "1m"
   reversed = c "7m"
   normal = c "0m"
}
{
    b = ""
    e = $0
    while (p = index(e, "\\")) {
      m = substr(e, p + 1, 1)
      b = b substr(e, 1, p - 1)
      if (m == "x") {
         b = b yellow substr(e, p + 2, 1) black
         e = substr(e, p + 3)
      }
      else if (m == "g") {
         b = b black
         e = substr(e, p + 2)
      }
      else if (m == "b") {
         b = b white
         e = substr(e, p + 2)
      }
      else if (m == "y") {
         b = b yellow
         e = substr(e, p + 2)
      }
      else if (m == "p") {
         b = b italic
         e = substr(e, p + 2)
      }
      else if (m == "r") {
         b = b bold
         e = substr(e, p + 2)
      }
      else if (m == "u") {
         b = b reversed
         e = substr(e, p + 2)
      }
      else if (m == "n") {
         b = b normal
         e = substr(e, p + 2)
      }
   }
   printf "%s", b e
   gsub("\\\\.","")
   if (length($0) < 80) printf "\n"
}

