#!/bin/sh

# Get appropriate directories from user:
DIR_NAME=$1 # A directory in /share like "adamant"

if test -z "$DIR_NAME"
then
  echo "DIR_NAME not specified as the first argument. This needs to be a directory in /share like 'adamant'." >&2
  exit 1
fi

# Only run if it is not already running...
ps aux | grep "unison -silent" | grep "$DIR_NAME" >/dev/null 2>/dev/null
if [ $? -eq 1 ]; then
   echo "Starting unison for $DIR_NAME."
   mkdir -p /home/user/.unison
   mkdir -p /home/user/$DIR_NAME
   # unison -silent -batch -fastcheck=true -logfile=/home/user/.unison/$DIR_NAME.log -times -prefer=/home/user/$DIR_NAME -repeat=watch /home/user/$DIR_NAME /share/$DIR_NAME >/home/user/.unison/$DIR_NAME.stdout 2>/home/user/.unison/$DIR_NAME.stderr &
   # Let's use a configuration file instead.
   unison -silent $DIR_NAME >/home/user/.unison/$DIR_NAME.stdout 2>/home/user/.unison/$DIR_NAME.stderr &
else
   echo "Unison already started for $DIR_NAME."
fi
