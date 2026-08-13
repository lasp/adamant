with Basic_Types;
with Test_Assembly_Ct_Stored_Products;

-- Static allocation of memory used to hold the two copies of the product store
-- for the test assembly.
package Test_Store_Memory_Ct is

   -- Note: The nominal subtypes must be left unconstrained (with the bounds coming
   -- from the initial values) so that 'Access of these objects can be passed to the
   -- component, whose Init expects access-to-unconstrained byte arrays.
   Store_Bytes_A : aliased Basic_Types.Byte_Array := [0 .. Test_Assembly_Ct_Stored_Products.Store_Size_In_Bytes - 1 => 0];
   Store_Bytes_B : aliased Basic_Types.Byte_Array := [0 .. Test_Assembly_Ct_Stored_Products.Store_Size_In_Bytes - 1 => 0];

end Test_Store_Memory_Ct;
