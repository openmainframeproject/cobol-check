#!/bin/bash
#
# Insert copyright notice at the top of each Java source file.
#
# 2020-12-22
# Dave Nicolette
#
shopt -s globstar
for filename in ./** ./..?* ; do
  if [ -f "$filename" ] ; then  
    if [[ "${filename: -5}" == ".java" ]]
    then
      IFS='\n' line=$(cat "$filename" | head -2 | tail -1)
      pattern="Copyright "
      if  [[ "$line" =~ $pattern ]]
      then 
        echo "File $filename already contains copyright notice" 
      else 
        echo "Inserting copyright notice into file $filename"
        cat copyright_notice.txt "$filename" > temp.txt
        mv temp.txt "$filename"
      fi
    fi
  fi
done
