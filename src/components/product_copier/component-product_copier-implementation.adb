--------------------------------------------------------------------------------
-- Product_Copier Component Implementation Body
--------------------------------------------------------------------------------

with Data_Product_Types; use Data_Product_Types;
with Data_Product_Enums;

package body Component.Product_Copier.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- At initialization, this component requires a list of source/destination pairs
   -- of data products to copy.
   --
   -- Init Parameters:
   -- Products_To_Copy : Product_Mapping_Array_Access - The list of mappings to be
   -- copied by this component every tick. Raises an error on Init if the list is
   -- null, as well as if two mappings share a destination.
   -- You must pass a reference (using 'Access) to this function, and as a
   -- consequence you can't declare your array of mappings within, say, a function's
   -- declarative region, since it must not be garbage collected. Either declare it
   -- in a package or use a dynamic allocation.
   -- Send_Event_On_Source_Id_Out_Of_Range : Boolean - When the status of a fetch is
   -- of Id_Out_Of_Range, specifies whether an error event should be sent. This could
   -- indicate misconfiguration, so sending error events is the default.
   -- Send_Event_On_Source_Not_Available : Boolean - When the status of a fetch is of
   -- Not_Available, specifies whether an error event should be sent. This might
   -- simply indicate that the product is not yet ready to be fetched, in which case
   -- this is expected behavior. Accordingly, not sending error events is the
   -- default.
   --
   overriding procedure Init (Self : in out Instance; Products_To_Copy : in not null Product_Mapping_Array_Access; Send_Event_On_Source_Id_Out_Of_Range : in Boolean := True; Send_Event_On_Source_Not_Available : in Boolean := False) is
   begin
      -- make sure no two destinations have the same ID, otherwise raise an error
      for I in Products_To_Copy'Range loop
         for J in I + 1 .. Products_To_Copy'Last loop
            pragma Assert (
               Products_To_Copy (I).Destination_Id /= Products_To_Copy (J).Destination_Id
            );
         end loop;
      end loop;

      -- copy configuration to component record
      Self.Send_Event_On_Source_Id_Out_Of_Range := Send_Event_On_Source_Id_Out_Of_Range;
      Self.Send_Event_On_Source_Not_Available := Send_Event_On_Source_Not_Available;
      Self.Mappings := Products_To_Copy;
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Triggers copying of data products (through request and send connectors).
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
   begin
      for Mapping of Self.Mappings.all loop
         declare
            use Data_Product_Enums;
            use Data_Product_Enums.Fetch_Status; -- required for `=` operator
            -- fetch source
            Dp_Return : constant Data_Product_Return.T :=
               Self.Data_Product_Fetch_T_Request ((Id => Mapping.Source_Id));
         begin
            case Dp_Return.The_Status is
               -- send error events if applicable
               when Fetch_Status.Not_Available =>
                  if Self.Send_Event_On_Source_Not_Available then
                     Self.Event_T_Send_If_Connected (
                        Self.Events.Source_Not_Available (
                           Self.Sys_Time_T_Get,
                           (
                              Tick => Arg.Count,
                              Mapping => Mapping
                           )
                        )
                     );
                  end if;
               when Fetch_Status.Id_Out_Of_Range =>
                  if Self.Send_Event_On_Source_Id_Out_Of_Range then
                     Self.Event_T_Send_If_Connected (
                        Self.Events.Source_Id_Out_Of_Range (
                           Self.Sys_Time_T_Get,
                           (
                              Tick => Arg.Count,
                              Mapping => Mapping
                           )
                        )
                     );
                  end if;

               -- send to dest
               when Fetch_Status.Success =>
                  Self.Data_Product_T_Send (Dp_Return.The_Data_Product);
            end case;
         end;
      end loop;
   end Tick_T_Recv_Sync;

end Component.Product_Copier.Implementation;
