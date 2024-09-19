--------------------------------------------------------------------------------
-- Event_Producer Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Event_Producer.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Tick to regulate the execution of the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Val : constant Natural := Self.Count mod 3;
   begin
      case Val is
         when 0 =>
            Self.Event_T_Send (Self.Events.Event_1 (Arg.Time, Arg));
         when 1 =>
            Self.Event_T_Send (Self.Events.Event_2 (Arg.Time, Arg));
         when 2 =>
            Self.Event_T_Send (Self.Events.Event_3 (Arg.Time, Arg));
         when others =>
            pragma Assert (False);
      end case;
      Self.Count := @ + 1;
   end Tick_T_Recv_Sync;

end Component.Event_Producer.Implementation;
