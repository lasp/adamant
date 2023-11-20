bin=build/bin/Linux/main.elf
dest=$ADAMANT_DIR/gnd/seq/run/seq_decode
redo-ifchange $bin
cp -f $bin $dest
echo "Published $bin to $dest." >&2
