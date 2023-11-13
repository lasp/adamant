--------------------------------------------------------------------------------
-- Data_Dependency_Component Component Implementation Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Sys_Time;

package body Component.Data_Dependency_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      use Data_Product_Enums;
      use Data_Product_Enums.Data_Dependency_Status;
      -- Get the current time:
      Current_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
      -- Define the temperature limit we are checking against:
      Temperature_Limit : constant Short_Float := 100.0;
      -- Data dependency value.
      Temp : Packed_F32.T;
      -- Data dependency timestamp:
      Ignore_Timestamp : Sys_Time.T;
      -- Data dependency value.
      Cnt : Packed_U16.T;
      Status : Data_Dependency_Status.E;
   begin
      -- First let's grab the counter data dependency and report its value.
      Status := Self.Get_Counter (
         Stale_Reference => Current_Time,
         Timestamp => Ignore_Timestamp,
         Value => Cnt
      );

      -- Check the status:
      case Status is
         when Success =>
            Put_Line ("The counter value is: " & Unsigned_16'Image (Cnt.Value));
         when Not_Available =>
            Put_Line ("The counter value is unavailable!");
         when Stale =>
            Put_Line ("The counter value is stale!");
         when Error =>
            null; -- Invalid_Data_Dependency will be called, no need to do anything more.
      end case;

      -- Grab the temperature and see if it is over the limit:
      Status := Self.Get_Temperature (
         Stale_Reference => Current_Time,
         Timestamp => Ignore_Timestamp,
         Value => Temp
      );

      -- Check the status:
      case Status is
         when Success =>
            if Temp.Value > Temperature_Limit then
               Put_Line ("The temperature value is too hot!");
            else
               Put_Line ("The temperature value is just right.");
            end if;
         when Not_Available =>
            Put_Line ("The temperature value is unavailable!");
         when Stale =>
            Put_Line ("The temperature value is stale!");
         when Error =>
            null; -- Invalid_Data_Dependency will be called, no need to do anything more.
      end case;
   end Tick_T_Recv_Sync;

   -----------------------------------------------
   -- Data dependency handlers:
   -----------------------------------------------
   -- Description:
   --    A set of data dependencies for the Data Dependency Component.
   -- Invalid data dependency handler. This procedure is called when a data dependency's id or length are found to be invalid:
   overriding procedure Invalid_Data_Dependency (Self : in out Instance; Id : in Data_Product_Types.Data_Product_Id; Ret : in Data_Product_Return.T) is
   begin
      Put_Line ("Oh, no! A data dependency was received that was malformed!");
   end Invalid_Data_Dependency;

end Component.Data_Dependency_Component.Implementation;
