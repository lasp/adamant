package Oo_Package.Tester is

   -- This child package has access to the parent package's private members.
   -- This is useful for unit testing since it is nice to inspect an object's
   -- internal private variables during testing, even though you don't want this
   -- feature during the deployment of your software.
   --
   -- This function retrieves the private value of "n" from within the object's
   -- record and returns it.
   function Get_N (Self : in Instance) return Integer;

end Oo_Package.Tester;
