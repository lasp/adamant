#!/bin/sh

assembly_events_file=$1

if test -z "$assembly_events_file"
then
  echo "usage: socket_event_decoder_to_bin.sh /path/to/assembly_events.py"
  echo "description: Convert a python script to a standalone executable with no dependencies that can run on any platform. You must provide the assembly events file that this program will be run with in production."
  exit 1
fi

# Get the python path of the program:
py_path=`which python`
echo $py_path
hidden_import=""
for dir in `python socket_event_decoder.py --python-path 0.0.0.0 3001 108 $assembly_events_file output.log`
do 
  py_path=$py_path:$dir
done
echo "PYTHONPATH: $py_path"
echo ""

# Get all modules that the program depends on:
for module in `python socket_event_decoder.py --module-dependencies 0.0.0.0 3001 108 $assembly_events_file output.log`
do 
  hidden_import=$hidden_import" --hidden-import $module"
done
echo "hidden imports: $hidden_import"
echo ""
 
# Run py installer.
exe="pyinstaller --onefile -y socket_event_decoder.py -p $py_path -F --distpath build/bin --specpath build/bin $hidden_import"
echo $exe
$exe
status=$?

if [ $status -eq 0 ]
then
  # Copy assembly events file for production use.
  cp $assembly_events_file build/bin/assembly_events.py
  echo "\n -------- \n The build was successful. \n The socket_event_decoder executable is located at build/bin/socket_event_decoder. \n For more help you can run 'build/bin/socket_event_decoder -h' \n --------"
  exit 0
else
  echo "\n -------- \n The build was unsuccessful, oh no! \n --------"
  exit 1
fi
