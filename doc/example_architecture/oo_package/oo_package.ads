package Oo_Package is

   -- Declare a tagged type. In Adamant, this is usually named
   -- "Instance".
   type Instance is tagged private;

   -- Declare an initialization procedure (a primitive operation)
   -- that sets the state of a variable called "N" inside of the tagged type.
   procedure Init (Self : in out Instance; N : in Integer);

   -- Create a primitive for our tagged type that adds N to a number.
   -- N is a state of instance that is stored upon initialization.
   function Add_N (Self : in Instance; Left : in Integer) return Integer;

-- Everything below this is private, and is only accessible
-- to child packages of this package.
private

   -- This is the private implementation of type instance. Instance
   -- is a record that contains a single private member variable, n.
   type Instance is tagged record
      N : Integer := 0;
   end record;

end Oo_Package;
