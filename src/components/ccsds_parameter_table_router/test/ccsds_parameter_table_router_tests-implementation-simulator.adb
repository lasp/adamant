with Ada.Real_Time;

package body Ccsds_Parameter_Table_Router_Tests.Implementation.Simulator is

   -- Sleep for a number of milliseconds:
   procedure Sleep (Ms : in Natural := 5) is
      use Ada.Real_Time;
      Sleep_Time : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (Ms);
      Wake_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Sleep_Time;
   begin
      delay until Wake_Time;
   end Sleep;

   -- Scenario control for the library-level task below:
   protected Sim_Control is
      procedure Arm (Ptr : in Class_Access);
      procedure Disarm;
      procedure Request_Quit;
      procedure Note_Idle;
      function Armed_Ptr return Class_Access;
      function Quit_Requested return Boolean;
      procedure Note_Exit;
      -- Blocks until the task has seen a Disarm and gone idle:
      entry Wait_Idle;
   private
      Armed : Class_Access := null;
      Idle : Boolean := True;
      Quit : Boolean := False;
   end Sim_Control;

   protected body Sim_Control is
      procedure Arm (Ptr : in Class_Access) is
      begin
         Armed := Ptr;
         Idle := False;
      end Arm;
      procedure Disarm is
      begin
         Armed := null;
      end Disarm;
      procedure Request_Quit is
      begin
         Quit := True;
      end Request_Quit;
      procedure Note_Idle is
      begin
         if Armed = null then
            Idle := True;
         end if;
      end Note_Idle;
      procedure Note_Exit is
      begin
         -- The task is quitting and will never touch the Tester again,
         -- so it counts as idle no matter what:
         Idle := True;
      end Note_Exit;
      function Armed_Ptr return Class_Access is (Armed);
      function Quit_Requested return Boolean is (Quit);
      entry Wait_Idle when Idle is
      begin
         null;
      end Wait_Idle;
   end Sim_Control;

   task Sim_Task;

   task body Sim_Task is
      Class_Self : Class_Access;
      Tick_Count : Natural := 0;
   begin
      loop
         if Sim_Control.Quit_Requested then
            Sim_Control.Note_Exit;
            exit;
         end if;
         Class_Self := Sim_Control.Armed_Ptr;

         if Class_Self = null then
            Sim_Control.Note_Idle;
            Sleep (2);
         elsif Task_Send_Response and then Task_Responses_To_Send > 0 then
            Sleep (4);
            -- Use schedule if available, otherwise use Task_Response_Status:
            declare
               Status_To_Send : Parameter_Enums.Parameter_Table_Update_Status.E;
            begin
               if Schedule_Length > 0 and then Schedule_Index < Schedule_Length then
                  Status_To_Send := Response_Schedule (Schedule_Index);
                  Schedule_Index := @ + 1;
               else
                  Status_To_Send := Task_Response_Status;
               end if;
               Class_Self.all.Tester.Parameters_Memory_Region_Release_T_Send ((
                  Region => (Address => Sim_Bytes'Address, Length => Sim_Bytes'Length),
                  Status => Status_To_Send
               ));
            end;
            Task_Responses_To_Send := @ - 1;
            if Task_Responses_To_Send = 0 then
               Task_Send_Response := False;
            end if;
         elsif Task_Send_Timeout then
            Sleep (4);
            Class_Self.all.Tester.Timeout_Tick_Send (((0, 0), 0));
            Tick_Count := @ + 1;
            if Tick_Count > 4 then
               Tick_Count := 0;
               Task_Send_Timeout := False;
            end if;
         else
            Sleep (2);
         end if;
      end loop;
   end Sim_Task;

   procedure Arm (Ptr : in Class_Access) is
   begin
      Sim_Control.Arm (Ptr);
   end Arm;

   procedure Disarm is
   begin
      Sim_Control.Disarm;
   end Disarm;

   procedure Request_Quit is
   begin
      Sim_Control.Request_Quit;
   end Request_Quit;

   procedure Wait_Idle is
   begin
      Sim_Control.Wait_Idle;
   end Wait_Idle;

end Ccsds_Parameter_Table_Router_Tests.Implementation.Simulator;
