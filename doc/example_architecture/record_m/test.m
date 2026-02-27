disp("create and disp example record:");
example1 = Example_Record(2, -1, 1, 0.5);
disp(example1);
disp("");
disp(example1.to_tuple_string());
disp("");
disp(example1.to_string());
disp("");
disp(example1.to_byte_string());
disp("");

disp("serializing and deserializing example record:");
data = example1.to_byte_array();
example2 = Example_Record.create_from_byte_array(data);
assert(example1 == example2);
disp("done.");
