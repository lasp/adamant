#!/bin/sh

directory=$1
output_directory=$2

if test -z "$directory"
then
  echo "usage: copy_all_matlab_autocode.sh /path/to/top/directory /path/to/output/directory"
  echo "description: Copy all matlab autocode found recursively from the provided directory to the output directory."
  exit 1
fi

if test -z "$output_directory"
then
  echo "usage: copy_all_matlab_autocode.sh /path/to/top/directory /path/to/output/directory"
  echo "description: Copy all matlab autocode found recursively from the provided directory to the output directory."
  exit 1
fi

list_get_matlab_files() {
  for look_dir in `redo $directory/print_path 2>&1 | awk 'NR>1{ print $0 }'` 
  do
    find $look_dir -maxdepth 4 -mindepth 2 -type f -name "*.m" | grep "build/m"
  done | uniq
}

mkdir -p $output_directory
for file in `list_get_matlab_files`
do
  cp -f $file $output_directory
done
