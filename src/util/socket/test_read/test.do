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

# Run socat to capture socket output:
port=`get_available_port`
test_file="test.dat"
rm -f $test_file
touch $test_file
echo "Starting socat to listen on port $port and print data to $test_file." >&2
socat TCP-LISTEN:$port - < expected.dat &

# Run test:
redo-ifchange build/bin/$TARGET/test.elf
./build/bin/$TARGET/test.elf 127.0.0.1 $port >&2
