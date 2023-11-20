with Command_Registration;
with Binary_Tree;
with Command_Types;

-- Package which contains a router table mapping command ids to
-- registration ids. A binary tree is used for the table with
-- O(n) insertion time and O(log n) search time.
--
-- Note: Duplicate entries in the table are not allowed and an
-- error will be returned if this is attempted.
package Router_Table is
   type Instance is tagged limited private;
   type Instance_Access is access all Instance;

   procedure Init (Self : in out Instance; Table_Size : Positive);
   procedure Destroy (Self : in out Instance);

   -- Add entry to the router table
   type Add_Status is (Success, Table_Full, Id_Conflict);
   function Add (Self : in out Instance; An_Entry : Command_Registration.U) return Add_Status;
   procedure Clear (Self : in out Instance);

   -- Get the router index from the table given an id:
   type Lookup_Status is (Success, Id_Not_Found);
   function Lookup_Registration_Id (Self : in Instance; Cmd_Id : in Command_Types.Command_Id; Registration_Id : out Command_Types.Command_Registration_Id) return Lookup_Status;

   -- Get functions:
   function Get_Size (Self : in Instance) return Natural;
   function Get_Capacity (Self : in Instance) return Positive;
private
   -- Define the Command Registration binary tree package:
   function Less_Than (Left, Right : Command_Registration.U) return Boolean;
   function Greater_Than (Left, Right : Command_Registration.U) return Boolean;
   package Cmd_Registration_B_Tree is new Binary_Tree (Command_Registration.U, Less_Than, Greater_Than);

   -- Define the router table type:
   type Instance is tagged limited record
      Table : Cmd_Registration_B_Tree.Instance;
   end record;
end Router_Table;
