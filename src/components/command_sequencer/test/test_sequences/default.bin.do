# Create output dirs:
output_dir=`dirname $1`
mkdir -p $output_dir
tmp_output_dir=`dirname $3`
mkdir -p $tmp_output_dir

# Form input name:
sequence=`basename $2`.txt

# Define configuration file:
config=test_cmd_tlm.config

# Track dependencies:
redo-ifchange $config $sequence

# Build the sequence:
cmd="seq_build --big --config $config -d $tmp_output_dir $sequence"
echo "$cmd" >&2
$cmd >&2

# Copy output to right place for redo:
cp $tmp_output_dir/`basename $1` $3
