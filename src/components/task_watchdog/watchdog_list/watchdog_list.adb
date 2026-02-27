
-- This is a somewhat generic, unprotected binary tree for holding apids and filter factors for the downsampler component.
package body Watchdog_List is

   -- Initialization of the task watchdog entry list
   procedure Init (Self : in out Instance; Task_Watchdog_Entry_Init_List : in Task_Watchdog_Init_List) is
      use Connector_Types;
   begin
      -- Make sure we have something in the list and it is not null
      pragma Assert (Task_Watchdog_Entry_Init_List'Length > 0, "Watchdog Init Entry list cannot be empty.");
      -- pragma assert
      Self.Task_Watchdog_Pet_Connections := new Task_Watchdog_Pet_List (Task_Watchdog_Entry_Init_List'First .. Task_Watchdog_Entry_Init_List'First + Task_Watchdog_Entry_Init_List'Length - 1);
      -- Save off the list so that we can use it in a protected fashion.
      for Index in Task_Watchdog_Entry_Init_List'Range loop
         Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Limit := Task_Watchdog_Entry_Init_List (Index).Max_Missed_Pet_Limit;
         Self.Task_Watchdog_Pet_Connections (Index).Critical := Task_Watchdog_Entry_Init_List (Index).Critical;
         Self.Task_Watchdog_Pet_Connections (Index).Action := Task_Watchdog_Entry_Init_List (Index).Action;
         Self.Task_Watchdog_Pet_Connections (Index).Action_Id := Task_Watchdog_Entry_Init_List (Index).Action_Id;
         Self.Task_Watchdog_Pet_Connections (Index).Petter_Has_Fault := Task_Watchdog_Entry_Init_List (Index).Petter_Has_Fault;
      end loop;
   end Init;

   procedure Reset_Pet_Count (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) is
   begin
      Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Count := 0;
   end Reset_Pet_Count;

   procedure Set_Pet_Limit (Self : in Instance; Index : in Connector_Types.Connector_Index_Type; New_Limit : in Missed_Pet_Limit_Type) is
   begin
      Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Limit := New_Limit;
      -- Reset the count as well
      Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Count := 0;
   end Set_Pet_Limit;

   procedure Set_Pet_Action (Self : in Instance; Index : in Connector_Types.Connector_Index_Type; New_Action : in Watchdog_Action_State.E) is
   begin
      Self.Task_Watchdog_Pet_Connections (Index).Action := New_Action;
   end Set_Pet_Action;

   function Check_Watchdog_Pets (Self : in Instance; Index : in Connector_Types.Connector_Index_Type; Is_Critical : out Boolean) return Check_Status is
      use Watchdog_Action_State;
      Pet_Count : constant Missed_Pet_Count_Type := Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Count;
      Pet_Count_Limit : constant Missed_Pet_Limit_Type := Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Limit;
      Return_Status : Check_Status := Petting;
      Pet_Action : constant Watchdog_Action_State.E := Self.Task_Watchdog_Pet_Connections (Index).Action;
   begin
      -- Set criticality to false until we know if we need to act on it
      Is_Critical := False;
      -- Make sure we are enabled to check this pet first, if not, return right away. (No need to do any more checking)
      if Pet_Action = Disabled then
         return Disable;
      end if;

      -- Check if the pet count is equal or larger than the limit to know if we need to take an action
      if Pet_Count >= Pet_Count_Limit then
         Return_Status := Repeat_Failure;
         Is_Critical := Self.Task_Watchdog_Pet_Connections (Index).Critical;

         -- Also check to see if we want to send an event or not and which action it is
         if Pet_Count = Pet_Count_Limit then
            case Pet_Action is
               when Disabled =>
                  -- Should never get here:
                  pragma Assert (False);
               when Warn =>
                  Return_Status := Warn_Failure;
               when Error_Fault =>
                  Return_Status := Fault_Failure;
            end case;
         end if;
      end if;

      -- Increment the count if needed
      if Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Count <= Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Limit then
         Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Count := @ + 1;
      end if;
      return Return_Status;
   end Check_Watchdog_Pets;

   function Get_Pet_Criticality (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Boolean is
   begin
      return Self.Task_Watchdog_Pet_Connections (Index).Critical;
   end Get_Pet_Criticality;

   function Get_Pet_Action (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Watchdog_Action_State.E is
   begin
      return Self.Task_Watchdog_Pet_Connections (Index).Action;
   end Get_Pet_Action;

   function Get_Pet_Action_Id (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Fault_Types.Fault_Id is
   begin
      return Self.Task_Watchdog_Pet_Connections (Index).Action_Id;
   end Get_Pet_Action_Id;

   function Get_Pet_Has_Fault (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Boolean is
   begin
      return Self.Task_Watchdog_Pet_Connections (Index).Petter_Has_Fault;
   end Get_Pet_Has_Fault;

   function Get_Pet_Count_Limit (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Missed_Pet_Limit_Type is
   begin
      return Self.Task_Watchdog_Pet_Connections (Index).Missed_Pet_Limit;
   end Get_Pet_Count_Limit;

end Watchdog_List;
