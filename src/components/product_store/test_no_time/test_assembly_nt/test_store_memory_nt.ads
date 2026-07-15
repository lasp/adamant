with Basic_Types;
with Test_Assembly_Nt_Stored_Products;

-- Static allocation of memory used to hold the product store for the test assembly.
package Test_Store_Memory_Nt is

   -- Note: The nominal subtype must be left unconstrained (with the bounds coming
   -- from the initial value) so that 'Access of this object can be passed to the
   -- component, whose Init expects an access-to-unconstrained byte array.
   Store_Bytes : aliased Basic_Types.Byte_Array := [0 .. Test_Assembly_Nt_Stored_Products.Store_Size_In_Bytes - 1 => 0];

end Test_Store_Memory_Nt;
