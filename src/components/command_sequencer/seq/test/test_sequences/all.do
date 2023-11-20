for seq in `ls *.txt`
do
  to_build=`basename $seq | sed 's/.txt$/.bin/g'`
  to_decode=`basename $seq`
  redo-ifchange build/bin/$to_build
  redo-ifchange build/decode/$to_decode
done
