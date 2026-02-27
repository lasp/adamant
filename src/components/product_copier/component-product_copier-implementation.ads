--------------------------------------------------------------------------------
-- Product_Copier Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;

-- Given two locations and a list of source/destination IDs, fetches Data_Product
-- entries from one location and sends/copies them to another upon receiving a
-- Tick.
-- The use case in mind was for the two locations to be databases (e.g.
-- Product_Database instances), and for this component to take snapshots of a
-- source database at a fixed interval. The idea is that if the values stored in
-- the source database is constantly in flux, then the destination database could
-- provide a stable view of the source -- within a tick, the values in the
-- destination database will not change between reads.
package Component.Product_Copier.Implementation is

   -- The component class instance record:
   type Instance is new Product_Copier.Base_Instance with private;

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
   overriding procedure Init (Self : in out Instance; Products_To_Copy : in not null Product_Mapping_Array_Access; Send_Event_On_Source_Id_Out_Of_Range : in Boolean := True; Send_Event_On_Source_Not_Available : in Boolean := False);

private

   -- The component class instance record:
   type Instance is new Product_Copier.Base_Instance with record
      Send_Event_On_Source_Id_Out_Of_Range : Boolean := True;
      Send_Event_On_Source_Not_Available : Boolean := False;
      Mappings : Product_Mapping_Array_Access := null;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Triggers copying of data products (through request and send connectors).
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

end Component.Product_Copier.Implementation;
