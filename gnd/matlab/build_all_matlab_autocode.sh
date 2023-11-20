#!/bin/sh

directory=$1

if test -z "$directory"
then
  echo "usage: build_all_matlab_autocode.sh /path/to/top/directory"
  echo "description: Build all matlab autocode found recursively from the provided directory."
  exit 1
fi

camel_case () {
  echo $1 | awk '
    # Capitalize first letter of a word
    function capp(word) {
      return toupper(substr(word,1,1)) tolower(substr(word,2))
    }
    
    # Replace spaces with "_" and capitalize each letter after "_"
    function camel(word,    len, str, idx, array) {
      gsub(" ", "_", word)
      if (index(word, "_") != 0) {
        len = split(word, array, "_")
        str = ""
        for (idx = 1; idx <= len; ++idx) {
          str = sprintf("%s_%s", str, capp(array[idx]))
        }
        return substr(str,2)
      }
      else {
        return capp(word)
      }
    }
    
    { print camel($0) }
  '
}

build_extension () {
  ext=$1
  new_ext=$2

  for look_dir in `redo $directory/print_path 2>&1 | awk 'NR>1{ print $0 }'` 
  do
    for yaml in `find $look_dir -maxdepth 1 -type f -name "*$ext"`
    do
      dir=`dirname $yaml`
      base=`basename $yaml $ext`
      camel_base=`camel_case $base`
      echo "$dir/build/m/$camel_base$new_ext"
    done
  done | xargs redo-ifchange
}

build_extension .record.yaml .m
build_extension .array.yaml .m
build_extension .enums.yaml ""
