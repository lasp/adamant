--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Reciprocal Base Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Includes:
with Sys_Time.Pretty;
with System.Address_Image;
with String_Util;
{% if tester_base_adb_includes %}
{% for include in tester_base_adb_includes %}
with {{ include }};
{% if include in ["Parameter_Enums"] %}
use {{ include }};
{% endif %}
{% endfor %}
{% endif %}

package body Component.{{ name }}_Reciprocal is

   ---------------------------------------
   -- Test Logging:
   ---------------------------------------
   procedure Set_Logger (Self : in out Base_Instance; Logger : in File_Logger.Instance_Access) is
   begin
      Self.Logger := Logger;
   end Set_Logger;

   procedure Log (Self : in out Base_Instance; String_To_Log : in String) is
      use File_Logger;
   begin
      if Self.Logger /= null then
         -- Make sure we have a log file to log to.
         Self.Logger.Log (String_Util.Trim_Both (Sys_Time.Pretty.Image (Self.System_Time)) & " " & String_To_Log);
      end if;
   end Log;

{% if connectors.invoker() %}
   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
{% for connector in connectors.invoker() %}
   not overriding function {{ connector.tester_name }}_Access (Self : in out Base_Instance; Index : in Connector_Index_Type := Connector_Index_Type'First) return not null {{ connector.tester_name }}_Connector.Invokee_Hook is
      subtype Index_Type is {{ connector.tester_name }}_Index;
   begin
      pragma Assert (Index = Index_Type'Last and then Index = Index_Type'First,
         "Cannot connect to invokee connector {{ connector.tester_name }} at index " & Connector_Index_Type'Image (Index) & ". " &
         "Index must be between " & Index_Type'Image (Index_Type'First) & " and " & Index_Type'Image (Index_Type'Last) & ".");
      if Self.Log_{{ connector.name }} then
         Self.Log ("-> {{ connector.tester_name }} returned at index " & String_Util.Trim_Both (Connector_Index_Type'Image (Index)) & " and address " & String_Util.Trim_Both (System.Address_Image ({{ connector.tester_name }}_Hook'Address)));
      end if;
      return {{ connector.tester_name }}_Hook'Access;
   end {{ connector.tester_name }}_Access;

{% if connector.kind == "get" %}
   function {{ connector.tester_name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Index : in Connector_Index_Type := Connector_Index_Type'First) return {{ connector.return_type }} is
{% elif connector.kind == "request" %}
   function {{ connector.tester_name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First) return {{ connector.return_type }} is
{% elif connector.kind == "provide" %}
   procedure {{ connector.tester_name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First) is
{% else %}
   function {{ connector.tester_name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First; Ignore : in Full_Queue_Action := Drop) return Connector_Types.Connector_Status is
{% endif %}
      Self : Base_Instance renames Base_Instance (Class_Self);
{% if connector.kind in ["get", "request"] %}
      Return_Type : {{ connector.return_type }};
{% endif %}
      -- Ignore the index, since we don't allocate arrayed connectors in the tester.
      Ignore_Index : Connector_Index_Type renames Index;
   begin
{% if connector.kind != "get" %}
      -- Log before connector call
      if Self.Log_{{ connector.name }} then
{% if connector.type_package in ["Event", "Data_Product", "Packet", "Fault", "Command"] %}
         if Self.Log_Verbose then
            -- Use the full representation to log the message.
            Self.Log ("-> {{ connector.name }} : " & {{ connector.type_package }}.Representation.To_Tuple_String (Arg));
         else
            -- Use the header representation to log the message.
            Self.Log ("-> {{ connector.name }} : " & {{ connector.type_package }}_Header.Representation.To_Tuple_String (Arg.Header));
         end if;
{% elif connector.type_package in ["Ccsds_Space_Packet"] %}
         if Self.Log_Verbose then
            -- Use the full representation to log the message.
            Self.Log ("-> {{ connector.name }} : " & Ccsds_Space_Packet.Representation.To_Tuple_String (Arg));
         else
            -- Use the header representation to log the message.
            Self.Log ("-> {{ connector.name }} : " & Ccsds_Primary_Header.Representation.To_Tuple_String (Arg.Header));
         end if;
{% else %}
{% if connector.type_model %}
         -- No header representation so just log the full generic package.
         Self.Log ("-> {{ connector.name }} : " & {{ connector.type_package }}.Representation.To_Tuple_String (Arg));
{% else %}
         -- Log the type
         Self.Log ("-> {{ connector.name }} :" & String_Util.Replace_White_Space ({{ connector.type }}'Image (Arg)));
{% endif %}
{% endif %}
      end if;
{% endif %}
      -- Call up to the derived class for execution
{% if connector.kind == "get" %}
      Return_Type := Base_Instance'Class (Self).{{ connector.tester_name }};
{% elif connector.kind == "request" %}
      Return_Type := Base_Instance'Class (Self).{{ connector.tester_name }} (Arg);
{% elif connector.kind == "provide" %}
      Base_Instance'Class (Self).{{ connector.tester_name }} (Arg);
{% else %}
      Base_Instance'Class (Self).{{ connector.tester_name }} (Arg);
      return Self.Connector_{{ connector.tester_name }}_Status;
{% endif %}
{% if connector.kind in ["get", "request", "provide"] %}
      -- Log after connector call
{% if connector.kind == "get" %}
{% set dir = "->" %}
{% set to_print = "Return_Type" %}
{% set finished = "" %}
{% set type = connector.return_type %}
{% set type_package = connector.return_type_package %}
{% set type_model = connector.return_type_model %}
{% elif connector.kind == "request" %}
{% set dir = "--" %}
{% set to_print = "Return_Type" %}
{% set finished = "finished " %}
{% set type = connector.return_type %}
{% set type_package = connector.return_type_package %}
{% set type_model = connector.return_type_model %}
{% elif connector.kind == "provide" %}
{% set dir = "--" %}
{% set to_print = "Arg" %}
{% set finished = "finished " %}
{% set type = connector.type %}
{% set type_package = connector.type_package %}
{% set type_model = connector.type_model %}
{% endif %}
      if Self.Log_{{ connector.name }} then
{% if type_package in ["Event", "Data_Product", "Packet", "Fault", "Command"] %}
         if Self.Log_Verbose then
            -- Use the full representation to log the message.
            Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & {{ type_package }}.Representation.To_Tuple_String ({{ to_print }}));
         else
            -- Use the header representation to log the message.
            Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & {{ type_package }}_Header.Representation.To_Tuple_String ({{ to_print }}.Header));
         end if;
{% elif type_package in ["Ccsds_Space_Packet"] %}
         if Self.Log_Verbose then
            -- Use the full representation to log the message.
            Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & Ccsds_Space_Packet.Representation.To_Tuple_String ({{ to_print }}));
         else
            -- Use the header representation to log the message.
            Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & Ccsds_Primary_Header.Representation.To_Tuple_String ({{ to_print }}.Header));
         end if;
{% else %}
{% if type_package in ["Parameter_Update"] and parameters and parameters.parameters %}
      -- Log parameter value
      if Arg.Operation /= Parameter_Enums.Parameter_Operation_Type.Update then
         Self.Log_Incoming_Parameter (Arg.Param);
      end if;
{% endif %}
{% if connector.return_type_package in ["Data_Product_Return"] and connector.type_package in ["Data_Product_Fetch"] and data_dependencies %}
         -- Now data dependency value
         Self.Log_Data_Dependencies (Arg, Return_Type);
{% endif %}
{% if type_model %}
         -- No header representation so just log the full generic package.
         Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & {{ type_package }}.Representation.To_Tuple_String ({{ to_print }}));
{% else %}
         -- Log the type
         Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}:" & String_Util.Replace_White_Space ({{ type }}'Image ({{ to_print }})));
{% endif %}
{% endif %}
      end if;
{% if connector.kind != "provide" %}
      return Return_Type;
{% endif %}
{% endif %}
   end {{ connector.tester_name }}_Hook;

{% endfor %}
{% endif %}
{% if connectors.invokee() %}
   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
{% for connector in connectors.invokee() %}
   not overriding procedure Attach_{{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; From_Index : in {{ connector.tester_name }}_Index{% endif %}; To_Component : in not null Component.Class_Access; Hook : in not null {{ connector.tester_name }}_Connector.Invokee_Hook; To_Index : in Connector_Index_Type := Connector_Index_Type'First) is
   begin
      Self.Connector_{{ connector.tester_name }}{% if connector.count == 0 or connector.count > 1 %} (From_Index){% endif %}.Attach (To_Component, Hook, To_Index);
      if Self.Log_{{ connector.name }} then
         Self.Log ("<- {{ connector.tester_name }} attached to index " & String_Util.Trim_Both (Connector_Index_Type'Image (To_Index)) & " and address " & String_Util.Trim_Both (System.Address_Image (Hook.all'Address)));
      end if;
   end Attach_{{ connector.tester_name }};

{% if connector.kind == "return" %}
   not overriding function {{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}) return {{ connector.return_type }} is
      Return_Type : {{ connector.return_type }};
{% elif connector.kind == "service" %}
   not overriding function {{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }} is
{% if connector.type_package in ["Parameter_Update"] and parameters and parameters.parameters %}
      use Parameter_Operation_Type;
{% endif %}
      Return_Type : {{ connector.return_type }};
{% elif connector.kind == "modify" %}
   not overriding procedure {{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) is
{% if connector.type_package in ["Parameter_Update"] and parameters and parameters.parameters %}
      use Parameter_Operation_Type;
{% endif %}
{% else %}
   not overriding procedure {{ connector.tester_name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}; Full_Queue_Behavior : in Full_Queue_Action := Drop) is
{% if connector.type_package in ["Parameter_Update"] and parameters and parameters.parameters %}
      use Parameter_Operation_Type;
{% endif %}
      Ret : Connector_Status;
{% endif %}
   begin
      -- Log before connector call:
{% if connector.kind != "return" %}
      if Self.Log_{{ connector.name }} then
{% if connector.type_package in ["Event", "Data_Product", "Packet", "Fault", "Command"] %}
         if Self.Log_Verbose then
            -- Use the full representation to log the message.
            Self.Log ("<- {{ connector.name }} : " & {{ connector.type_package }}.Representation.To_Tuple_String (Arg));
         else
            -- Use the header representation to log the message.
            Self.Log ("<- {{ connector.name }} : " & {{ connector.type_package }}_Header.Representation.To_Tuple_String (Arg.Header));
         end if;
{% elif connector.type_package in ["Ccsds_Space_Packet"] %}
         if Self.Log_Verbose then
            -- Use the full representation to log the message.
            Self.Log ("<- {{ connector.name }} : " & Ccsds_Space_Packet.Representation.To_Tuple_String (Arg));
         else
            -- Use the header representation to log the message.
            Self.Log ("<- {{ connector.name }} : " & Ccsds_Primary_Header.Representation.To_Tuple_String (Arg.Header));
         end if;
{% else %}
{% if connector.type_model %}
         -- No header representation so just log the full item.
         Self.Log ("<- {{ connector.name }} : " & {{ connector.type_package }}.Representation.To_Tuple_String (Arg));
{% else %}
         -- Log the type
         Self.Log ("<- {{ connector.name }} :" & String_Util.Replace_White_Space ({{ connector.type }}'Image (Arg)));
{% endif %}
{% endif %}
      end if;
{% if connector.type_package in ["Command"] and commands and commands.commands %}
      Self.Log_Incoming_Command (Arg);
{% endif %}
{% endif %}
      -- Call connector
{% if connector.kind == "return" %}
      Return_Type := {{ connector.tester_name }}_Connector.Call (Self.Connector_{{ connector.tester_name }}{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %});
{% elif connector.kind == "service" %}
      Return_Type := {{ connector.tester_name }}_Connector.Call (Self.Connector_{{ connector.tester_name }}{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %}, Arg);
{% elif connector.kind == "modify" %}
      {{ connector.tester_name }}_Connector.Call (Self.Connector_{{ connector.tester_name }}{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %}, Arg);
{% else %}
      Ret := {{ connector.tester_name }}_Connector.Call (Self.Connector_{{ connector.tester_name }}{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %}, Arg, Full_Queue_Behavior);
      case Ret is
         -- Send worked fine, just log and continue on.
         when Success =>
            if Self.Log_{{ connector.name }} then
               -- Log that the call has been finished
               Self.Log ("-- {{ connector.name }} finished.");
            end if;
         when Message_Dropped =>
{% if connector.kind == "recv_async" %}
            -- The message was dropped. Call up to the message dropped handler:
            {{ connector.tester_name }}_Dropped (Base_Instance'Class (Self){% if connector.count == 0 or connector.count > 1 %}, Index{% endif %}, Arg);
{% else %}
            pragma Assert (False, "A message should never be dropped from a synchronous connection.");
{% endif %}
      end case;
{% endif %}
{% if connector.kind in ["return", "service", "modify"] %}
{% if connector.kind == "return" %}
{% set dir = "<-" %}
{% set to_print = "Return_Type" %}
{% set finished = "" %}
{% set type = connector.return_type %}
{% set type_package = connector.return_type_package %}
{% set type_model = connector.return_type_model %}
{% elif connector.kind == "service" %}
{% set dir = "--" %}
{% set to_print = "Return_Type" %}
{% set finished = "finished " %}
{% set type = connector.return_type %}
{% set type_package = connector.return_type_package %}
{% set type_model = connector.return_type_model %}
{% elif connector.kind == "modify" %}
{% set dir = "--" %}
{% set to_print = "Arg" %}
{% set finished = "finished " %}
{% set type = connector.type %}
{% set type_package = connector.type_package %}
{% set type_model = connector.type_model %}
{% endif %}
      -- Log after connector call
      if Self.Log_{{ connector.name }} then
{% if type_package in ["Event", "Data_Product", "Packet", "Fault", "Command"] %}
         if Self.Log_Verbose then
            -- Use the full representation to log the message.
            Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & {{ type_package }}.Representation.To_Tuple_String ({{ to_print }}));
         else
            -- Use the header representation to log the message.
            Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & {{ type_package }}_Header.Representation.To_Tuple_String ({{ to_print }}));
         end if;
{% elif type_package in ["Ccsds_Space_Packet"] %}
         if Self.Log_Verbose then
            -- Use the full representation to log the message.
            Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & Ccsds_Space_Packet.Representation.To_Tuple_String ({{ to_print }}));
         else
            -- Use the header representation to log the message.
            Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & Ccsds_Primary_Header.Representation.To_Tuple_String ({{ to_print }}));
         end if;
{% else %}
{% if type_package in ["Parameter_Update"] and parameters and parameters.parameters %}
         if Arg.Operation /= Parameter_Enums.Parameter_Operation_Type.Update then
            Self.Log_Incoming_Parameter (Arg.Param);
         end if;
{% endif %}
{% if type_model %}
         -- No header representation so just log the full item.
         Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & {{ type_package }}.Representation.To_Tuple_String ({{ to_print }}));
{% else %}
         -- Log the type
         Self.Log ("{{ dir }} {{ connector.name }} {{ finished }}: " & String_Util.Replace_White_Space ({{ type }}'Image ({{ to_print }})));
{% endif %}
{% endif %}
      end if;
{% if connector.kind != "modify" %}
      return Return_Type;
{% endif %}
{% endif %}
   end {{ connector.tester_name }};

{% endfor %}
{% endif %}
   ---------------------------------------------------
   -- Definition of cycle function for task execution:
   ---------------------------------------------------
   -- Passive component queue implementation for cycle.
   -- Tester components NEVER have threads.
   -- This method is implemented, but if called will throw an assertion.
   overriding procedure Cycle (Self : in out Base_Instance) is
      Ignore : Base_Instance renames Self;
   begin
      -- This is a passive component, meaning it CANNOT be tasked. If the component
      -- is given a task we quit.
      pragma Assert (False);
   end Cycle;

{% if parameters and ("Parameter.T" in connectors.of_kind("recv_sync").types) %}
   -- Send special zero-id parameter to component signaling it to update its staged
   -- parameters to its local parameters whenever it is ready.
   procedure Update_Local_Parameters (Self : in out Base_Instance) is
      Zero_Param : constant Parameter.T := (Header => (Id => 0, Buffer_Length => 0), Buffer => [others => 0]);
   begin
      Self.Parameter_T_Send (Zero_Param);
   end Update_Local_Parameters;

{% endif %}
{% if events %}
   -------------------------------------------
   -- Event handling primitives:
   -------------------------------------------
   -- Dispatch event to correct execution handler:
   not overriding procedure Dispatch_Event (Self : in out Base_Instance; Evnt : in Event.T) is
      Local_Id : constant {{ events.name }}.Local_Event_Id_Type := {{ events.name }}.Local_Event_Id_Type'Val (Evnt.Header.Id);
      Dispatch_To : constant Dispatch_Event_Procedure := Event_Id_Table (Local_Id);
   begin
      Dispatch_To (Self, Evnt);
   end Dispatch_Event;

{% for event in events %}
   not overriding procedure Dispatch_{{ event.name }} (Self : in out Base_Instance; Evnt : in Event.T) is
{% if event.type %}
{% if event.type_model %}
      package Param_Deserializer renames {{ event.type_package }}.Serialization;
      Event_Param_Deserialized : constant {{ event.type_package }}.T := Param_Deserializer.From_Byte_Array (Evnt.Param_Buffer (Evnt.Param_Buffer'First .. Evnt.Param_Buffer'First + Param_Deserializer.Serialized_Length - 1));
{% else %}
      package Param_Deserializer is new Serializer ({{ event.type }});
      Event_Param_Deserialized : constant {{ event.type }} := Param_Deserializer.From_Byte_Array (Evnt.Param_Buffer (Evnt.Param_Buffer'First .. Evnt.Param_Buffer'First + Param_Deserializer.Serialized_Length - 1));
{% endif %}

{% else %}
      Ignore : Event.T renames Evnt;
{% endif %}
   begin
      -- Write the data to the file if event logging is enabled and the event package is known.
      if Self.Log_Events and then Self.Log_{{ event.name }} then
{% if event.type %}
         Self.Log ("    Event.{{ event.name }} : " & {{ event.type_package }}.Representation.To_Tuple_String (Event_Param_Deserialized));
{% else %}
         Self.Log ("    Event.{{ event.name }}");
{% endif %}
      end if;
      -- Deserialize the arguments and call up to the derived class for execution.
      Base_Instance'Class (Self).{{ event.name }}{% if event.type %} (Event_Param_Deserialized){% endif %};
   end Dispatch_{{ event.name }};

{% endfor %}
{% endif %}
{% if data_products %}
   -------------------------------------------
   -- Data product handling primitives:
   -------------------------------------------
   -- Dispatch data product to correct execution handler:
   not overriding procedure Dispatch_Data_Product (Self : in out Base_Instance; Dp : in Data_Product.T) is
      Local_Id : constant {{ data_products.name }}.Local_Data_Product_Id_Type := {{ data_products.name }}.Local_Data_Product_Id_Type'Val (Dp.Header.Id);
      Dispatch_To : constant Dispatch_Data_Product_Procedure := Data_Product_Id_Table (Local_Id);
   begin
      Dispatch_To (Self, Dp);
   end Dispatch_Data_Product;

{% for dp in data_products %}
   not overriding procedure Dispatch_{{ dp.name }} (Self : in out Base_Instance; Dp : in Data_Product.T) is
{% if dp.type_model %}
      package Data_Product_Deserializer renames {{ dp.type_package }}.Serialization;
      Dp_Param_Deserialized : constant {{ dp.type_package }}.T := Data_Product_Deserializer.From_Byte_Array (Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Data_Product_Deserializer.Serialized_Length - 1));
{% else %}
      package Data_Product_Deserializer is new Serializer ({{ dp.type }});
      Dp_Param_Deserialized : constant {{ dp.type }} := Data_Product_Deserializer.From_Byte_Array (Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Data_Product_Deserializer.Serialized_Length - 1));
{% endif %}
   begin
      -- Write the data to the file if data product logging is enabled
      if Self.Log_Data_Products and then Self.Log_{{ dp.name }} then
         Self.Log ("    Data_Product.{{ dp.name }} : " & {{ dp.type_package }}.Representation.To_Tuple_String (Dp_Param_Deserialized));
      end if;
      -- Deserialize the arguments and call up to the derived class for execution.
      Base_Instance'Class (Self).{{ dp.name }} (Dp_Param_Deserialized);
   end Dispatch_{{ dp.name }};

{% endfor %}
{% endif %}
{% if packets %}
   -------------------------------------------
   -- Packet handling primitives:
   -------------------------------------------
   -- Dispatch packet to correct execution handler:
   not overriding procedure Dispatch_Packet (Self : in out Base_Instance; P : in Packet.T) is
   begin
      -- Use case statement for dispatching, instead of a table because the
      -- packet ids are not guaranteed to be contiguous:
      case P.Header.Id is
{% for p in packets %}
{% if packets.ids %}
         when {{ p.id }} => Self.Dispatch_{{ p.name }} (P);
{% else %}
         when {{ loop.index0 }} => Self.Dispatch_{{ p.name }} (P);
{% endif %}
{% endfor %}
         when others => pragma Assert (False, "Unexpected packet id received!");
      end case;
   end Dispatch_Packet;

{% for p in packets %}
   not overriding procedure Dispatch_{{ p.name }} (Self : in out Base_Instance; P : in Packet.T) is
{% if p.type %}
{% if p.type_model %}
      package Packet_Deserializer renames {{ p.type_package }}.Serialization;
{% if not p.type_model.variable_length %}
      Packet_Param_Deserialized : constant {{ p.type_package }}.T := Packet_Deserializer.From_Byte_Array (P.Buffer (P.Buffer'First .. P.Buffer'First + Packet_Deserializer.Serialized_Length - 1));
{% endif %}
{% else %}
      package Packet_Deserializer is new Serializer ({{ p.type }});
      Packet_Param_Deserialized : constant {{ p.type_package }}.T := Packet_Deserializer.From_Byte_Array (P.Buffer (P.Buffer'First .. P.Buffer'First + Packet_Deserializer.Serialized_Length - 1));
{% endif %}
{% endif %}
   begin
{% if p.type %}
{% if p.type_model and p.type_model.variable_length %}
      declare
         use Serializer_Types;
         Item : {{ p.type }};
      begin
         if Packet_Deserializer.From_Byte_Array (Item, P.Buffer) /= Success then
            -- Deserialization failed, so let's just manually copy what we can:
            declare
               use Byte_Array_Util;
               -- Overlay type with properly sized byte array:
               subtype Sized_Byte_Array_Index is Natural range 0 .. P.Header.Buffer_Length - 1;
               subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
               Overlay : Sized_Byte_Array with Import, Convention => Ada, Address => Item'Address;
            begin
               -- Safely copy the buffer to the overlaid type:
               Safe_Left_Copy (Overlay, P.Buffer (P.Buffer'First .. P.Buffer'First + P.Header.Buffer_Length - 1));
            end;
         end if;
         -- Write the data to the file we have open
         if Self.Log_Packets and then Self.Log_{{ p.name }} then
            Self.Log ("    Packet.{{ p.name }} : " & {{ p.type_package }}.Representation.To_Tuple_String (Item));
         end if;
         Base_Instance'Class (Self).{{ p.name }} (Item);
      end;
{% else %}
         if Self.Log_Packets and then Self.Log_{{ p.name }} then
            Self.Log ("    Packet.{{ p.name }} : " & {{ p.type_package }}.Representation.To_Tuple_String (Packet_Param_Deserialized));
         end if;
      -- Deserialize the arguments and call up to the derived class for execution.
      Base_Instance'Class (Self).{{ p.name }} (Packet_Param_Deserialized);
{% endif %}
{% else %}
      if Self.Log_Packets and then Self.Log_{{ p.name }} then
         Self.Log ("    Packet.{{ p.name }} : " & Basic_Types.Representation.To_Tuple_String (P.Buffer));
      end if;
      -- Packet has no defined type, just pass it up without deserializing into a specific type:
      Base_Instance'Class (Self).{{ p.name }} (P);
{% endif %}
   end Dispatch_{{ p.name }};

{% endfor %}
{% endif %}
{% if faults %}
   -------------------------------------------
   -- Fault handling primitives:
   -------------------------------------------
   -- Dispatch fault to correct execution handler:
   not overriding procedure Dispatch_Fault (Self : in out Base_Instance; Flt : in Fault.T) is
   begin
      -- Use case statement for dispatching, instead of a table because the
      -- fault ids are not guaranteed to be contiguous:
      case Flt.Header.Id is
{% for fault in faults %}
{% if faults.ids %}
         when {{ fault.id }} => Self.Dispatch_{{ fault.name }} (Flt);
{% else %}
         when {{ loop.index0 }} => Self.Dispatch_{{ fault.name }} (Flt);
{% endif %}
{% endfor %}
         when others => pragma Assert (False, "Unexpected fault id received!");
      end case;
   end Dispatch_Fault;
{% for fault in faults %}
   not overriding procedure Dispatch_{{ fault.name }} (Self : in out Base_Instance; Flt : in Fault.T) is
{% if fault.type %}
{% if fault.type_model %}
      package Param_Deserializer renames {{ fault.type_package }}.Serialization;
      Fault_Param_Deserialized : constant {{ fault.type_package }}.T := Param_Deserializer.From_Byte_Array (Flt.Param_Buffer (Flt.Param_Buffer'First .. Flt.Param_Buffer'First + Param_Deserializer.Serialized_Length - 1));
{% else %}
      package Param_Deserializer is new Serializer ({{ fault.type }});
      Fault_Param_Deserialized : constant {{ fault.type }} := Param_Deserializer.From_Byte_Array (Flt.Param_Buffer (Flt.Param_Buffer'First .. Flt.Param_Buffer'First + Param_Deserializer.Serialized_Length - 1));
{% endif %}
{% else %}
      Ignore : Fault.T renames Flt;
{% endif %}
   begin
      -- Write the fault data to the file if the type is known
      if Self.Log_Faults and then Self.Log_{{ fault.name }} then
{% if fault.type %}
         Self.Log ("    Fault.{{ fault.name }} : " & {{ fault.type_package }}.Representation.To_Tuple_String (Fault_Param_Deserialized));
{% else %}
         Self.Log ("    Fault.{{ fault.name }}");
{% endif %}
      end if;
      -- Deserialize the arguments and call up to the derived class for execution.
      Base_Instance'Class (Self).{{ fault.name }}{% if fault.type %} (Fault_Param_Deserialized){% endif %};
   end Dispatch_{{ fault.name }};

{% endfor %}
{% endif %}
{% if data_dependencies %}
   procedure Log_Data_Dependencies (Self : in out Base_Instance; Arg : in Data_Product_Fetch.T; Dp : in Data_Product_Return.T) is
   begin
      -- Search using the ID and log that data dependency
      case Arg.Id is
{% for dd in data_dependencies %}
         -- Length for {{ dd.name }}:
         when {{ loop.index0 }} =>
            declare
{% if dd.type_model %}
               package Data_Dependency_Deserializer renames {{ dd.type_package }}.Serialization;
               Dd_Param_Deserialized : constant {{ dd.type_package }}.T := Data_Dependency_Deserializer.From_Byte_Array (Dp.The_Data_Product.Buffer (Dp.The_Data_Product.Buffer'First .. Dp.The_Data_Product.Buffer'First + Data_Dependency_Deserializer.Serialized_Length - 1));
{% else %}
               package Data_Dependency_Deserializer is new Serializer ({{ dd.type }});
               Dd_Param_Deserialized : constant {{ dd.type }} := Data_Dependency_Deserializer.From_Byte_Array (Dp.The_Data_Product.Buffer (Dp.The_Data_Product.Buffer'First .. Dp.The_Data_Product.Buffer'First + Data_Dependency_Deserializer.Serialized_Length - 1));
{% endif %}
            begin
{% if dd.type_model %}
               Self.Log ("    Data_Dependency.{{ dd.name }} : " & {{ dd.type_package }}.Representation.To_Tuple_String (Dd_Param_Deserialized));
{% else %}
               Self.Log ("    Data_Dependency.{{ dd.name }}");
{% endif %}
            end;
{% endfor %}
         -- Do not fill. The ID is not recognized.
         when others =>
            Self.Log ("    Data Dependency ID " & String_Util.Trim_Both (Data_Product_Id.Representation.Id_To_Tuple_String (Arg.Id)) & " not recognized.");
      end case;
   end Log_Data_Dependencies;

{% endif %}
{% if parameters and parameters.parameters %}
   -- Procedure that will log the specific parameters
   procedure Log_Incoming_Parameter (Self : in out Base_Instance; Param : in Parameter.T) is
      use Parameter_Types;
      Local_Id : {{ parameters.name }}.Local_Parameter_Id_Type;
   begin
      if Self.Log_Parameters then
         if Natural (Param.Header.Id) <= {{ parameters.name }}.Num_Parameters and then Param.Header.Id >= {{ parameters.name }}.Get_Id_Base (Self.Parameters) then
            Local_Id := {{ parameters.name }}.Local_Parameter_Id_Type'Val (Param.Header.Id - {{ parameters.name }}.Get_Id_Base (Self.Parameters));
            case Local_Id is
{% for parameter in parameters %}
               when {{ parameters.name }}.{{ parameter.name }}_Id =>
                  declare
                     -- Log the specific parameter that was found
{% if parameter.type_model %}
                     package Param_Deserializer renames {{ parameter.type_package }}.Serialization;
                     Param_Deserialized : constant {{ parameter.type_package }}.T := Param_Deserializer.From_Byte_Array (Param.Buffer (Param.Buffer'First .. Param.Buffer'First + Param_Deserializer.Serialized_Length - 1));
{% else %}
                     Ignore : Parameter.T renames Param;
{% endif %}
                  begin
                     if Self.Log_{{ parameter.name }} then
{% if parameter.type_model %}
                        Self.Log ("    Parameter.{{ parameter.name }} : " & {{ parameter.type_package }}.Representation.To_Tuple_String (Param_Deserialized));
{% else %}
                        Self.Log ("    Parameter.{{ parameter.name }}");
{% endif %}
                     end if;
                  end;
{% endfor %}
            end case;
         else
               Self.Log ("    Parameter ID " & String_Util.Trim_Both (Parameter_Id.Representation.Id_To_Tuple_String (Param.Header.Id)) & " not recognized.");
         end if;
      end if;
   end Log_Incoming_Parameter;

{% endif %}
{% if commands and commands.commands %}
   -- Procedure that will log the specific command
   procedure Log_Incoming_Command (Self : in out Base_Instance; Cmd : in Command.T) is
      use Command_Types;
      Local_Id : {{ commands.name }}.Local_Command_Id_Type;
   begin
      if Self.Log_Commands then
         if Natural (Cmd.Header.Id) <= {{ commands.name }}.Num_Commands then
            Local_Id := {{ commands.name }}.Local_Command_Id_Type'Val (Cmd.Header.Id - {{ commands.name }}.Get_Id_Base (Self.Commands));
            case Local_Id is
{% for command in commands %}
               when {{ commands.name }}.{{command.name}}_Id =>
                  declare
{% if command.type_model %}
                     package Param_Deserializer renames {{ command.type_package }}.Serialization;
{% if not command.type_model.variable_length %}
                     Cmd_Param_Deserialized : constant {{ command.type_package }}.T := Param_Deserializer.From_Byte_Array (Cmd.Arg_Buffer (Cmd.Arg_Buffer'First .. Cmd.Arg_Buffer'First + Param_Deserializer.Serialized_Length - 1));
{% else %}
                     use Serializer_Types;
                     Stat : Serialization_Status;
                     Num_Bytes_Deserialized : Natural;
                     Args : {{ command.type }};
{% endif %}
{% else %}
                     Ignore : Command.T renames Cmd;
{% endif %}
                  begin
{% if command.type_model %}
{% if not command.type_model.variable_length %}
                     if Self.Log_{{ command.name }} then
                        Self.Log ("    Command.{{ command.name }} : " & {{ command.type_package }}.Representation.To_Tuple_String (Cmd_Param_Deserialized));
                     end if;
{% else %}
                     if Self.Log_{{ command.name }} then
                        Stat := Param_Deserializer.From_Byte_Array (Args, Cmd.Arg_Buffer (Cmd.Arg_Buffer'First .. Cmd.Arg_Buffer'First + Cmd.Header.Arg_Buffer_Length - 1), Num_Bytes_Deserialized);
                        if Stat = Success and then Cmd.Header.Arg_Buffer_Length >= Num_Bytes_Deserialized then
                           Self.Log ("    Command.{{ command.name }} : " & {{ command.type_package }}.Representation.To_Tuple_String (Args));
                        else
                           Self.Log ("    Command.{{ command.name }} : Could not deserialize arguments");
                        end if;
                     end if;
{% endif %}
{% else %}
                     if Self.Log_{{ command.name }} then
                        Self.Log ("    Command.{{ command.name }}");
                     end if;
{% endif %}
                  end;
{% endfor %}
            end case;
         else
               Self.Log ("    Command ID " & String_Util.Trim_Both (Command_Id.Representation.Id_To_Tuple_String (Cmd.Header.Id)) & " not recognized.");
         end if;
      end if;
   end Log_Incoming_Command;

{% endif %}
end Component.{{ name }}_Reciprocal;
