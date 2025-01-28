# Sys_Time with 16-bit Subseconds

To enable this Sys_Time representation in your system, add the following to your `env/activate`:

```
export REMOVE_BUILD_PATH=$HOME/adamant/src/types/sys_time
export EXTRA_BUILD_PATH=$HOME/adamant/src/types/sys_time/subseconds_16
```

which will remove the standard Sys_Time packed record from the build path, and add the Sys_Time with 16-bit subseconds into the build path instead.
