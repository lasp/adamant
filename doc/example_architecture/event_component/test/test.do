bin=build/bin/Linux_Test/test.elf
redo-ifchange $bin
$bin 1>&2 | true
