config=../test_assembly/build/txt/test_assembly_seq_cmd_tlm.txt
signature_file=signature_file.config

redo-ifchange $config $signature_file

cat $config > $3

echo "; Define the sequence signature file" >> $3
echo "sigFile $signature_file" >> $3
