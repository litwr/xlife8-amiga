awk --non-decimal-data '{
   p = match($0,"S01:00[0-9A-F]{6}")
   if (p) {
       n = 0 + ("0x" substr($0, p + 4, 8)) + ("0x'$2'")
       s = "S01:" sprintf("%08x", n)
       sub("S01:00[0-9A-F]{6}", s)
   }
   print
}' $1
