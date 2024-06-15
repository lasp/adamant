--------------------------------------------------------------------------------
-- Product_Copier Component Implementation Body
--------------------------------------------------------------------------------

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
   overriding procedure Init (Self : in out Instance; Products_To_Copy : in Product_Mapping_Array_Access; Send_Event_On_Source_Id_Out_Of_Range : in Boolean := True; Send_Event_On_Source_Not_Available : in Boolean := False) is
      -- TODO declarations
   begin
      null; -- TODO statements
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Triggers copying of data products (through request and send connectors).
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- TODO declarations
   begin
      null; -- TODO statements
   end Tick_T_Recv_Sync;

end Component.Product_Copier.Implementation;
