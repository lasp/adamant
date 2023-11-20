export TARGET="`uname`_Test"

# File name definitions:
test_file="test.dat"
rm -f $test_file

# Run test:
redo-ifchange build/bin/$TARGET/test.elf
output=`./build/bin/$TARGET/test.elf > $test_file`
echo "$output" >&2

if `echo "$output" | grep "FAIL" > /dev/null`
then
  return 1
fi

# Make sure that expected result is captured: 
redo-ifchange expected.dat
diff $test_file expected.dat
