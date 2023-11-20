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
test_file="tosend.dat"
rm -f $config_file

# Generate test file:
redo-ifchange $test_file

# Run socat to capture socket output:
port=`get_available_port`
echo $port > $config_file
echo "Starting socat to listen on port $port and send data from $test_file." >&2
socat TCP-LISTEN:$port - < $test_file &
sleep 0.5

# Run test:
redo-ifchange build/bin/$TARGET/test.elf
output=`./build/bin/$TARGET/test.elf "$@" 127.0.0.1 $port`
echo "$output" >&2

if `echo "$output" | grep "FAIL" > /dev/null`
then
  return 1
fi
