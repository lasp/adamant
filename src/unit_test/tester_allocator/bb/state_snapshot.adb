-- Bareboard body: save the object's bytes into a static buffer once,
-- and copy them back on Restore. See the spec for why this is sound.
with Basic_Types;

package body State_Snapshot is

   -- Static snapshot storage, sized to the object:
   Size_In_Bytes : constant Natural := Object_Type'Object_Size / Basic_Types.Byte'Object_Size;
   Snapshot : Basic_Types.Byte_Array (0 .. Size_In_Bytes - 1);
   Saved : Boolean := False;

   procedure Save (Object : in Object_Type) is
      Object_Bytes : Basic_Types.Byte_Array (Snapshot'Range)
         with Import, Convention => Ada, Address => Object'Address;
   begin
      if not Saved then
         Snapshot := Object_Bytes;
         Saved := True;
      end if;
   end Save;

   procedure Restore (Object : in out Object_Type) is
      Object_Bytes : Basic_Types.Byte_Array (Snapshot'Range)
         with Import, Convention => Ada, Address => Object'Address;
   begin
      pragma Assert (Saved, "State_Snapshot.Restore called before Save.");
      Object_Bytes := Snapshot;
   end Restore;

end State_Snapshot;
