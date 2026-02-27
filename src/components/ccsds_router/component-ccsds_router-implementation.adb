--------------------------------------------------------------------------------
-- Ccsds_Router Component Implementation Body
--------------------------------------------------------------------------------

with Interfaces;

package body Component.Ccsds_Router.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a routing table which maps CCSDS packet APIDs to a list of output connector indexes. This is provided as part of the initialization function.
   --
   -- Init Parameters:
   -- table : Ccsds_Router_Types.Router_Table_Entry_Array - An array of router table entries which include routing and sequence count checking information.
   -- Report_Unrecognized_Apids : Boolean - Should the component report unrecognized APIDs by sending out an error packet and event, True, or should it not report them at all, False.
   --
   overriding procedure Init (Self : in out Instance; Table : in Ccsds_Router_Types.Router_Table_Entry_Array; Report_Unrecognized_Apids : in Boolean := True) is
   begin
      -- Allocate space for the table:
      Self.Table.Init (Table'Length);

      -- Store elements in the table:
      for Table_Entry of Table loop
         -- Make sure that the destinations are in range of the arrayed connector index:
         if Table_Entry.Destinations /= null then
            for Destination_Index of Table_Entry.Destinations.all loop
               pragma Assert (Destination_Index >= Self.Connector_Ccsds_Space_Packet_T_Send'First and then Destination_Index <= Self.Connector_Ccsds_Space_Packet_T_Send'Last,
                   "Destination index for APID '" & Ccsds_Primary_Header.Ccsds_Apid_Type'Image (Table_Entry.Apid) & " is out of range: " & Connector_Index_Type'Image (Destination_Index) &
                   ". Must be between '" & Connector_Index_Type'Image (Self.Connector_Ccsds_Space_Packet_T_Send'First) & "' and '" &
                   Connector_Index_Type'Image (Self.Connector_Ccsds_Space_Packet_T_Send'Last) & "'.");
            end loop;
         end if;

         declare
            The_Entry : constant Internal_Router_Table_Entry := (Table_Entry => Table_Entry, Last_Sequence_Count => Ccsds_Primary_Header.Ccsds_Sequence_Count_Type'Last);
            Ignore_1 : Internal_Router_Table_Entry;
            Ignore_2 : Natural;
            Ret : Boolean;
         begin
            -- Make sure the APID is not already stored in our table:
            Ret := Self.Table.Search (The_Entry, Ignore_1, Ignore_2);
            pragma Assert (not Ret, "Duplicate APID '" & Ccsds_Primary_Header.Ccsds_Apid_Type'Image (Table_Entry.Apid) & "' not allowed in router table!");
            -- Add entry to the table:
            Ret := Self.Table.Add (The_Entry);
            pragma Assert (Ret, "Router table too small to hold entry. This should never happen unless there is a bug.");
         end;
      end loop;

      -- Store the unrecognized apid behavior:
      Self.Report_Unrecognized_Apids := Report_Unrecognized_Apids;
   end Init;

   not overriding procedure Final (Self : in out Instance) is
   begin
      Self.Table.Destroy;
   end Final;

   ---------------------------------------
   -- Binary tree comparison operators:
   ---------------------------------------
   function Less_Than (Left, Right : Internal_Router_Table_Entry) return Boolean is
      use Ccsds_Primary_Header;
   begin
      return Left.Table_Entry.Apid < Right.Table_Entry.Apid;
   end Less_Than;
   function Greater_Than (Left, Right : Internal_Router_Table_Entry) return Boolean is
      use Ccsds_Primary_Header;
   begin
      return Left.Table_Entry.Apid > Right.Table_Entry.Apid;
   end Greater_Than;

   -- Produce a warning if the sequence count is not as expected:
   procedure Warn_Sequence_Count (Self : in out Instance; Internal_Table_Entry : in Internal_Router_Table_Entry; Element_Index : in Positive; Header : in Ccsds_Primary_Header.T) is
      use Ccsds_Primary_Header;
      use Interfaces;
      Expected_Sequence_Count : constant Ccsds_Sequence_Count_Type := Internal_Table_Entry.Last_Sequence_Count + 1;
   begin
      -- If sequence count is not as expected, throw event:
      if Header.Sequence_Count /= Expected_Sequence_Count then
         Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Sequence_Count_Received (Self.Sys_Time_T_Get, (
            Ccsds_Header => Header,
            Received_Sequence_Count => Unsigned_16 (Header.Sequence_Count),
            Expected_Sequence_Count => Unsigned_16 (Expected_Sequence_Count)
         )));
      end if;

      -- Store the updated last sequence count:
      declare
         New_Table_Entry : Internal_Router_Table_Entry := Internal_Table_Entry;
      begin
         New_Table_Entry.Last_Sequence_Count := Header.Sequence_Count;
         Self.Table.Set (Element_Index, New_Table_Entry);
      end;
   end Warn_Sequence_Count;

   ---------------------------------------
   -- Helper functions:
   ---------------------------------------

   procedure Drop_Packet (Self : in out Instance; Arg : in Ccsds_Space_Packet.T; Evt : in Event.T) is
   begin
      -- Throw the event:
      Self.Event_T_Send_If_Connected (Evt);
      -- Forward the packet out as an error packet:
      Self.Packet_T_Send_If_Connected (Self.Packets.Error_Packet_Truncate (Evt.Header.Time, Arg));
   end Drop_Packet;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The synchronous ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      Table_Entry_To_Find : constant Internal_Router_Table_Entry := ((Arg.Header.Apid, null, No_Check), 0);
      Table_Entry_Found : Internal_Router_Table_Entry;
      Found_Entry_Index : Natural;
   begin
      -- Search for the router table entry associated with the given APID:
      if Self.Table.Search (Table_Entry_To_Find, Table_Entry_Found, Found_Entry_Index) then
         -- We found an entry, now if the destinations are valid, route to them:
         declare
            Table_Entry : Router_Table_Entry renames Table_Entry_Found.Table_Entry;
            Route_To_Destination : Boolean := True;
         begin
            -- Check sequence count if required:
            case Table_Entry.Sequence_Count_Mode is
               when No_Check =>
                  null;
               when Warn =>
                  -- Warn via event if unexpected sequence count found:
                  Self.Warn_Sequence_Count (Table_Entry_Found, Found_Entry_Index, Arg.Header);
               when Drop_Dupes =>
                  -- Warn via event if unexpected sequence count found:
                  Self.Warn_Sequence_Count (Table_Entry_Found, Found_Entry_Index, Arg.Header);
                  -- Check for duplicate; report and drop if necessary:
                  declare
                     use Ccsds_Primary_Header;
                     Last_Sequence_Count : Ccsds_Primary_Header.Ccsds_Sequence_Count_Type renames Table_Entry_Found.Last_Sequence_Count;
                  begin
                     if Arg.Header.Sequence_Count = Last_Sequence_Count then
                        Self.Drop_Packet (Arg, Self.Events.Dropped_Duplicate_Packet (Self.Sys_Time_T_Get, Arg.Header));
                        Route_To_Destination := False;
                     end if;
                  end;
            end case;

            -- Route to destinations:
            if Route_To_Destination then
               if Table_Entry.Destinations /= null then
                  for Destination of Table_Entry.Destinations.all loop
                     Self.Ccsds_Space_Packet_T_Send_If_Connected (Destination, Arg);
                  end loop;
               end if;
            end if;
         end;
      else
         -- Unrecognized APID, if the unrecognized connector is attached then
         -- forward these packets out of this connector, otherwise, report
         -- an error.
         if Self.Is_Unrecognized_Ccsds_Space_Packet_T_Send_Connected then
            Self.Unrecognized_Ccsds_Space_Packet_T_Send (Arg);
         end if;

         -- If we are supposed to report unrecognized APIDs then do so.
         if Self.Report_Unrecognized_Apids then
            Self.Drop_Packet (Arg, Self.Events.Unrecognized_Apid (Self.Sys_Time_T_Get, Arg.Header));
         end if;

      end if;
   end Ccsds_Space_Packet_T_Recv_Sync;

   -- The asynchronous ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      -- Call the synchronous connector handler:
      Self.Ccsds_Space_Packet_T_Recv_Sync (Arg);
   end Ccsds_Space_Packet_T_Recv_Async;

   -- This procedure is called when a Ccsds_Space_Packet_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      Self.Drop_Packet (Arg, Self.Events.Dropped_Packet (Self.Sys_Time_T_Get, Arg.Header));
   end Ccsds_Space_Packet_T_Recv_Async_Dropped;

end Component.Ccsds_Router.Implementation;
