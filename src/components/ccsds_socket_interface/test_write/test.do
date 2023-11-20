export TARGET="`uname`_Test"

# Get a free TCP port:
get_available_port()
{
  python -c '
import socket
s=socket.socket()
s.bind(("", 0))
print(s.getsockname()[1])
s.close()'
}

# File name definitions:
config_file="socket_config.txt"
test_file="test.dat"
rm -f $test_file
rm -f $config_file

# Run socat to capture socket output:
port=`get_available_port`
touch $test_file
echo $port > $config_file
echo "Starting socat to listen on port $port and print data to $test_file." >&2
socat TCP-LISTEN:$port FILE:$test_file &

# Run test:
redo-ifchange build/bin/$TARGET/test.elf
output=`./build/bin/$TARGET/test.elf "$@" 127.0.0.1 $port`
echo "$output" >&2

if `echo "$output" | grep "FAIL" > /dev/null`
then
  return 1
fi

# Make sure that expected result is captured: 
redo-ifchange expected.dat
diff $test_file expected.dat
