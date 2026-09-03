with Ada.Real_Time;

package body Memory_Copier_Tests.Implementation.Simulator is

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
         elsif Task_Send_Response then
            -- Send a valid response:
            Class_Self.all.Tester.Timeout_Tick_Send (((0, 0), 0)); -- send occasional timeout for coverage reasons
            Sleep (4);
            Class_Self.all.Tester.Memory_Region_Release_T_Send ((Region => (Address => Sim_Bytes'Address, Length => Sim_Bytes'Length), Status => Task_Response));
            Task_Send_Response := False;
         elsif Task_Send_Timeout then
            -- Send a valid response:
            Sleep (4);
            Class_Self.all.Tester.Timeout_Tick_Send (((0, 0), 0));
            Tick_Count := @ + 1;
            if Tick_Count > 4 then
               Tick_Count := 0;
               Task_Send_Timeout := False;
            end if;
         else
            -- Sleep:
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

end Memory_Copier_Tests.Implementation.Simulator;
