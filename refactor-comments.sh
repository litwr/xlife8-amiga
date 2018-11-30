sed -rz 's/\n[ \t]*;;[ \t]*([^\n]*)\n([^;][^\n]*)\n/\n\2\t\t;;\1\n/g' $1 >zzt
sed -rz 's/\n[ \t]*;;[ \t]*([^\n]*)\n([^;][^\n]*)\n/\n\2\t\t;;\1\n/g' zzt >$2
rm zzt
