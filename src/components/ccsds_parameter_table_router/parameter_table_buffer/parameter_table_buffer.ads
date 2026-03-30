with Basic_Types;
with Parameter_Types;
with Ccsds_Enums;
with Memory_Region;

package Parameter_Table_Buffer is

   -- The staging buffer instance:
   type Instance is tagged limited private;

   -- Create and destroy the table buffer object
   procedure Create (Self : in out Instance; Buffer_Size : in Positive);
   procedure Destroy (Self : in out Instance);

   -- Return status from the Append operation:
   type Append_Status is (
      Packet_Ignored,
      Buffering_Table,
      New_Table,
      Complete_Table,
      Too_Small_Table,
      Buffer_Overflow
   );

   -- Append packet data to the buffer. The sequence flag is used to
   -- determine if this is the beginning, middle, or end of the table.
   function Append (
      Self : in out Instance;
      Data : in Basic_Types.Byte_Array;
      Sequence_Flag : in Ccsds_Enums.Ccsds_Sequence_Flag.E
   ) return Append_Status;

   -- Getters

   -- Return table region with length set to full size of current table
   function Get_Table_Region (Self : in Instance) return Memory_Region.T;

   -- Return table region with length set to full size of buffer
   function Get_Full_Buffer_Region (Self : in Instance) return Memory_Region.T;

   -- Return table ID, length, and number of packets received for current table
   function Get_Table_Id (Self : in Instance) return Parameter_Types.Parameter_Table_Id;
   function Get_Table_Length (Self : in Instance) return Natural;
   function Get_Packet_Count (Self : in Instance) return Natural;

private

   type Buffer_State is (Idle, Receiving_Table);

   type Instance is tagged limited record
      Buffer : Basic_Types.Byte_Array_Access := null;
      Buffer_Index : Natural := 0;
      Table_Id : Parameter_Types.Parameter_Table_Id := 0;
      State : Buffer_State := Idle;
      Packet_Count : Natural := 0;
   end record;

end Parameter_Table_Buffer;
