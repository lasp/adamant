--------------------------------------------------------------------------------
-- Event_Text_Logger Component Tester Spec
--------------------------------------------------------------------------------

-- Standard Includes:
with Component.Event_Text_Logger_Reciprocal;
with Event_Assembly_Event_To_Text;

-- The Event Text Logger component receives events on an asynchronous queue and prints them either to the terminal or to a file as it receives them. The print statements for events are generated per the assembly that the Event Text Logger is attached to. By pointing the generator to a particular assembly model, you enable it to recognize and print certain events that are present in that assembly.
package Component.Event_Text_Logger.Implementation.Tester is

   -- Component class instance:
   type Instance is new Component.Event_Text_Logger_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Event_Text_Logger.Implementation.Instance (Event_To_Text => Event_Assembly_Event_To_Text.Event_To_Text'Access);
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Event_T_Send_Dropped : Boolean := False;
      Event_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : Positive := 1) return Natural;

end Component.Event_Text_Logger.Implementation.Tester;
