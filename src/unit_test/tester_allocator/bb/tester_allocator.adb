package body Tester_Allocator is

   -- Single shared static storage. AUnit serializes test execution,
   -- so reusing one Tester instance across scenarios is correct -- but
   -- the user's Set_Up_Test must call Init_Base/Init/Connect to clean
   -- per-scenario state, just as it would for a freshly heap-allocated
   -- Tester on the host.
   --
   -- Located at the (instantiated) package body level rather than
   -- inside Allocate so it counts as a library-level object under
   -- Jorvik's No_Local_Protected_Objects restriction.
   Storage : aliased Tester_Inst;
   -- Pre-converted access; qualifying the attribute at declaration
   -- avoids the "argument of conversion cannot be access attribute"
   -- error you get from `Tester_Access (Storage'Unchecked_Access)`.
   Storage_Ref : constant Tester_Access := Storage'Unchecked_Access;

   function Allocate return Tester_Access is
   begin
      return Storage_Ref;
   end Allocate;

   procedure Free (T : in out Tester_Access) is
   begin
      -- Static storage -- nothing to free. Null the user's handle
      -- so dangling-reference behavior matches the host body.
      T := null;
   end Free;

end Tester_Allocator;
