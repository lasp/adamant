# Create output dirs:
output_dir=`dirname $1`
mkdir -p $output_dir
tmp_output_dir=`dirname $3`
mkdir -p $tmp_output_dir

# Form input name:
compiled_sequence=build/bin/`basename $2`.bin
source=`basename $2`.txt

# Define configuration file:
config=seq_cmd_tlm.config

# Track dependencies:
redo-ifchange $config $compiled_sequence

# Decode the sequence:
cmd="seq_decode --config $config -d $tmp_output_dir $compiled_sequence"
echo "$cmd" >&2
echo "Compiled on: `date`"
echo "Source: $source"
echo "Binary: $compiled_sequence"
echo "Decode File (this file): $1"
echo ""
echo ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
echo ";; Decoded Sequence: " 
echo ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
echo ""
$cmd
echo ""
echo ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
echo ";; Sequence Source (LASEL):"
echo ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
echo ""
cat $source
