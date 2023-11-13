ls *.txt | xargs redo-ifchange
get_name () {
  awk '

  /^\s*seq/ {
    print $2
  }

  ' $1
}

create_sig () {
  awk '

  /^\s*seq/ {
    print $0
  }
  /^\s*argument/ {
    print $0
  }

  ' $1
}

for seq in `ls *.txt`
do
  echo "; Signature for $seq"
  name=`get_name $seq`
  filename=`basename $seq .txt`
  if [ "$name" = "$filename" ]; then
    create_sig $seq
  else
    echo "Sequence file '$seq' contains a sequence with name '$name'. The filename and sequence name must match!" >&2
    exit 1
  fi
  echo ""
done
