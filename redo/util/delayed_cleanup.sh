#!/bin/sh

# Given an adamant temporary build directory (session directory), this script
# removes that directory when it detects that it is no longer being used. Top
# level redo targets create a *.running file in the session directory that they
# remove when they are done executing. There are times when a *.running file
# may not appear in the directory because redo is in between build targets. 
# This only happens briefly, but we do NOT want to remove the session directory
# if this is the case. To make this removal robust to that circumstance it first
# waits 2 seconds, and then it checks 7 time for *.running files at different
# time intervals. If it never detects a *.running file in the directory it
# assumes that the temporary directory is no longer being used, and it removes
# it. 
#
# Because the temporary directories contain large caches, if we do not remove
# them aggressively from RAM the system will quickly fill up and the Adamant
# build system will be unable to compile code or will slow down significantly.
#
# This script is run by each high level redo target at the end of its build
# task. Because of the *.running detection described above, only the last
# redo target for a parent redo target will actually remove the temporary
# directory, after no other subprocesses are using it. This is the desired
# behavior and is safe.

# Get appropriate directory from user:
session_tmp_dir=$1 # something like /tmp/tmp.ABCDEFG.adamant/687402
if test -z "$session_tmp_dir"
then
  echo "session_tmp_dir not specified as the first argument." >&2
  exit 1
fi

# Sleep for 2 seconds.
sleep 2

# If the directory exists then run the following if statements to determine
# if it is still being used.
if [ -d "$session_tmp_dir" ]; then
  # Make sure no build process is currently running in the directory.
  ls -U $session_tmp_dir/*.running >/dev/null 2>/dev/null
  if [ $? -ne 0 ]; then
    # Do this again to make sure we didn't get fooled. It is possible that
    # we are in the exact time between redo processes running in this temp
    # directory, and in that case there may not be a .running file
    sleep 0.5
    ls -U $session_tmp_dir/*.running >/dev/null 2>/dev/null
    if [ $? -ne 0 ]; then
  
      # Do this again to make sure we didn't get fooled. It is possible that
      # we are in the exact time between redo processes running in this temp
      # directory, and in that case there may not be a .running file
      sleep 0.2
      ls -U $session_tmp_dir/*.running >/dev/null 2>/dev/null
      if [ $? -ne 0 ]; then
  
        # Do this again to make sure we didn't get fooled. It is possible that
        # we are in the exact time between redo processes running in this temp
        # directory, and in that case there may not be a .running file
        sleep 0.8
        ls -U $session_tmp_dir/*.running >/dev/null 2>/dev/null
        if [ $? -ne 0 ]; then
  
          # Do this again to make sure we didn't get fooled. It is possible that
          # we are in the exact time between redo processes running in this temp
          # directory, and in that case there may not be a .running file
          sleep 0.5
          ls -U $session_tmp_dir/*.running >/dev/null 2>/dev/null
          if [ $? -ne 0 ]; then
  
            # Do this again to make sure we didn't get fooled. It is possible that
            # we are in the exact time between redo processes running in this temp
            # directory, and in that case there may not be a .running file
            sleep 0.7
            ls -U $session_tmp_dir/*.running >/dev/null 2>/dev/null
            if [ $? -ne 0 ]; then
  
              # Do this again to make sure we didn't get fooled. It is possible that
              # we are in the exact time between redo processes running in this temp
              # directory, and in that case there may not be a .running file
              sleep 0.3
              ls -U $session_tmp_dir/*.running >/dev/null 2>/dev/null
              if [ $? -ne 0 ]; then
                # We can be pretty sure that the temp directory is no longer being used
                # by a running build process. Remove it if it still exists
                if [ -d "$session_tmp_dir" ]; then
                  echo "removing $session_tmp_dir"
                  rm -rf $session_tmp_dir
                fi
              fi
            fi
          fi
        fi
      fi
    fi
  fi
fi
