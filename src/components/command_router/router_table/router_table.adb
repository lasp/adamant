package body Router_Table is

   procedure Init (Self : in out Instance; Table_Size : Positive) is
   begin
      Self.Table.Init (Table_Size);
   end Init;

   procedure Destroy (Self : in out Instance) is
   begin
      Self.Table.Destroy;
   end Destroy;

   ------------------------------------------------------------
   -- Private functions for the binary tree package definition:
   ------------------------------------------------------------

   function Less_Than (Left, Right : Command_Registration.U) return Boolean is
      use Command_Types;
   begin
      return Left.Command_Id < Right.Command_Id;
   end Less_Than;

   function Greater_Than (Left, Right : Command_Registration.U) return Boolean is
      use Command_Types;
   begin
      return Left.Command_Id > Right.Command_Id;
   end Greater_Than;

   ------------------------------------------------------------
   -- Public functions:
   ------------------------------------------------------------

   function Add (Self : in out Instance; An_Entry : Command_Registration.U) return Add_Status is
      Ignore_1 : Command_Registration.U;
      Ignore_2 : Natural;
   begin
      -- Make sure the command Id is not already in the table:
      if Self.Table.Search (An_Entry, Ignore_1, Ignore_2) then
         return Id_Conflict;
      end if;

      -- Add the registration to the table:
      if not Self.Table.Add (An_Entry) then
         return Table_Full;
      end if;
      return Success;
   end Add;

   procedure Clear (Self : in out Instance) is
   begin
      Self.Table.Clear;
   end Clear;

   function Lookup_Registration_Id (Self : in Instance; Cmd_Id : in Command_Types.Command_Id; Registration_Id : out Command_Types.Command_Registration_Id) return Lookup_Status is
      Ignore : Positive;
      Registration_To_Find : constant Command_Registration.U := (Registration_Id => 0, Command_Id => Cmd_Id);
      Registration_Found : Command_Registration.U;
   begin
      -- Using ID, binary search the table ranges to find the correct entry:
      if not Self.Table.Search (Registration_To_Find, Registration_Found, Ignore) then
         Registration_Id := Command_Types.Command_Registration_Id'Last;
         return Id_Not_Found;
      end if;
      Registration_Id := Registration_Found.Registration_Id;
      return Success;
   end Lookup_Registration_Id;

   function Get_Size (Self : in Instance) return Natural is
   begin
      return Self.Table.Get_Size;
   end Get_Size;

   function Get_Capacity (Self : in Instance) return Positive is
   begin
      return Self.Table.Get_Capacity;
   end Get_Capacity;

end Router_Table;
