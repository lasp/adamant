--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Tester Body
--------------------------------------------------------------------------------
{% if tester_template_adb_includes %}

-- Includes:
{% for include in tester_template_adb_includes %}
with {{ include }};
{% endfor %}
{% endif %}
{% if connectors.n_arrayed().invokee() %}

with Safe_Deallocator;
{% endif %}

package body Component.{{ name }}.Implementation.Tester is

{% if init_base or connectors.invoker() or events or data_products or faults %}
   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance{% if init_base.parameters %}; {{ init_base.parameter_declaration_string() }}{% endif %}) is
   begin
{% if init_base %}
      -- Initialize component heap:
      Self.Component_Instance.Init_Base{% if init_base.parameters %} ({% for n in init_base.parameter_names %}{{ n }} => {{ n }}{{ ", " if not loop.last }}{% endfor %}){% endif %};

{% endif %}
      -- Initialize tester heap:
{% if connectors.invoker() %}
      -- Connector histories:
{% for connector in connectors.invoker() %}
      Self.{{ connector.tester_name }}_History.Init (Depth => 100);
{% endfor %}
{% endif %}
{% if connectors.invokee() %}
{% for connector in connectors.invokee() %}
{% if connector.count == 0   %}
      -- Allocated the arrayed connector for the tester invoker connector {{ connector.tester_name }}. We
      -- make this the same size as allocated for the component under test.
      if {{ connector.name }}_Count > 0 then
         Self.Connector_{{ connector.tester_name }} := new {{ connector.tester_name }}_Array ({{ connector.tester_name }}_Index'First .. {{ connector.tester_name }}_Index'First + {{ connector.name }}_Count - 1);
      end if;
{% endif %}
{% endfor %}
{% endif %}
{% if events %}
      -- Event histories:
{% for event in events %}
      Self.{{ event.name }}_History.Init (Depth => 100);
{% endfor %}
{% endif %}
{% if data_products %}
      -- Data product histories:
{% for dp in data_products %}
      Self.{{ dp.name }}_History.Init (Depth => 100);
{% endfor %}
{% endif %}
{% if packets %}
      -- Packet histories:
{% for p in packets %}
      Self.{{ p.name }}_History.Init (Depth => 100);
{% endfor %}
{% endif %}
{% if faults %}
      -- Fault histories:
{% for fault in faults %}
      Self.{{ fault.name }}_History.Init (Depth => 100);
{% endfor %}
{% endif %}
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
{% if connectors.n_arrayed().invokee() %}
{% for connector in connectors.n_arrayed().invokee() %}
      procedure Free_{{ connector.tester_name }}_Array is new Safe_Deallocator.Deallocate_If_Testing (
         Object => {{ connector.tester_name }}_Array,
         Name => {{ connector.tester_name }}_Array_Access
      );
{% endfor %}
{% endif %}
   begin
      -- Destroy tester heap:
{% if connectors.invoker() %}
      -- Connector histories:
{% for connector in connectors.invoker() %}
      Self.{{ connector.tester_name }}_History.Destroy;
{% endfor %}
{% endif %}
{% if events %}
      -- Event histories:
{% for event in events %}
      Self.{{ event.name }}_History.Destroy;
{% endfor %}
{% endif %}
{% if data_products %}
      -- Data product histories:
{% for dp in data_products %}
      Self.{{ dp.name }}_History.Destroy;
{% endfor %}
{% endif %}
{% if packets %}
      -- Packet histories:
{% for p in packets %}
      Self.{{ p.name }}_History.Destroy;
{% endfor %}
{% endif %}
{% if faults %}
      -- Fault histories:
{% for fault in faults %}
      Self.{{ fault.name }}_History.Destroy;
{% endfor %}
{% endif %}
{% if connectors.n_arrayed().invoker() or connectors.of_kind("recv_async") %}

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
{% endif %}
{% if connectors.n_arrayed().invokee() %}
{% for connector in connectors.n_arrayed().invokee() %}
      if Self.Connector_{{ connector.tester_name }} /= null then
         Free_{{ connector.tester_name }}_Array (Self.Connector_{{ connector.tester_name }});
      end if;
{% endfor %}
{% endif %}
   end Final_Base;

{% endif %}
{% if connectors %}
   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
{% for connector in connectors.invoker() %}
{% if connector.count > 1 %}
{% for index in range(0, connector.count) %}
      Self.Component_Instance.Attach_{{ connector.name }} (From_Index => {{ index + 1 }}, To_Component => Self'Unchecked_Access, Hook => Self.{{ connector.tester_name }}_Access);
{% endfor %}
{% elif connector.count == 0 %}
{% for index in range(0, 3) %}
      Self.Component_Instance.Attach_{{ connector.name }} (From_Index => {{ index + 1 }}, To_Component => Self'Unchecked_Access, Hook => Self.{{ connector.tester_name }}_Access);
{% endfor %}
{% else %}
      Self.Component_Instance.Attach_{{ connector.name }} (To_Component => Self'Unchecked_Access, Hook => Self.{{ connector.tester_name }}_Access);
{% endif %}
{% endfor %}
{% for connector in connectors.invokee() %}
{% if connector.count > 1 %}
      for Idx in Self.Connector_{{ connector.tester_name }}'Range loop
         Self.Attach_{{ connector.tester_name }} (From_Index => Idx, To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.{{ connector.name }}_Access (Index => Idx), To_Index => Idx);
      end loop;
{% elif connector.count == 0 %}
      for Idx in Self.Connector_{{ connector.tester_name }}.all'Range loop
         Self.Attach_{{ connector.tester_name }} (From_Index => Idx, To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.{{ connector.name }}_Access (Index => Idx), To_Index => Idx);
      end loop;
{% else %}
      Self.Attach_{{ connector.tester_name }} (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.{{ connector.name }}_Access);
{% endif %}
{% endfor %}
   end Connect;

{% endif %}
{% if connectors.invoker() %}
{% if data_dependencies %}
   -- Helper function for returning data dependencies:
   function Return_Data_Dependency (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T is
      use Data_Product_Types;
      use Data_Product_Enums.Fetch_Status;
      use Sys_Time;
      -- Set default return values. These will be overridden below based on test configuration and
      -- the ID requested.
      Id_To_Return : Data_Product_Types.Data_Product_Id := Self.Data_Dependency_Return_Id_Override;
      Length_To_Return : Data_Product_Types.Data_Product_Buffer_Length_Type := Self.Data_Dependency_Return_Length_Override;
      Return_Status : Data_Product_Enums.Fetch_Status.E := Self.Data_Dependency_Return_Status_Override;
      Buffer_To_Return : Data_Product_Types.Data_Product_Buffer_Type;
      Time_To_Return : Sys_Time.T := Self.Data_Dependency_Timestamp_Override;
   begin
      -- Determine return data product ID:
      if Id_To_Return = 0 then
         case Arg.Id is
{% for dd in data_dependencies %}
            -- ID for {{ dd.name }}:
            when {{ loop.index0 }} => Id_To_Return := {{ loop.index0 }};
{% endfor %}
            -- If ID can not be found, then return ID out of range error.
            when others =>
               if Return_Status = Data_Product_Enums.Fetch_Status.Success then
                  Return_Status := Data_Product_Enums.Fetch_Status.Id_Out_Of_Range;
               end if;
         end case;
      end if;

      -- Determine return data product length:
      if Length_To_Return = 0 then
         case Arg.Id is
{% for dd in data_dependencies %}
            -- Length for {{ dd.name }}:
            when {{ loop.index0 }} => Length_To_Return := {{ dd.type_package }}.Size_In_Bytes;
{% endfor %}
            -- If ID can not be found, then return ID out of range error.
            when others =>
               if Return_Status = Data_Product_Enums.Fetch_Status.Success then
                  Return_Status := Data_Product_Enums.Fetch_Status.Id_Out_Of_Range;
               end if;
         end case;
      end if;

      -- Determine return timestamp:
      if Time_To_Return = (0, 0) then
         Time_To_Return := Self.System_Time;
      end if;

      -- Fill the data product buffer:
      if Return_Status = Data_Product_Enums.Fetch_Status.Success then
         case Arg.Id is
{% for dd in data_dependencies %}
            -- Length for {{ dd.name }}:
            when {{ loop.index0 }} =>
               Buffer_To_Return (Buffer_To_Return'First .. Buffer_To_Return'First + {{ dd.type_package }}.Size_In_Bytes - 1) :=
                  {{ dd.type_package }}.Serialization.To_Byte_Array (Self.{{ dd.name }});
{% endfor %}
            -- Do not fill. The ID is not recognized.
            when others =>
               Return_Status := Data_Product_Enums.Fetch_Status.Id_Out_Of_Range;
         end case;
      end if;

      -- Return the data product with the status:
      return (
         The_Status => Return_Status,
         The_Data_Product => (
            Header => (
               Time => Time_To_Return,
               Id => Id_To_Return,
               Buffer_Length => Length_To_Return
            ),
            Buffer => Buffer_To_Return
         )
      );
   end Return_Data_Dependency;

{% endif %}
   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
{% for connector in connectors.invoker() %}
{% if connector.description %}
{{ printMultiLine(connector.description, '   -- ') }}
{% endif %}
{% if connector.kind == "get" %}
   overriding function {{ connector.tester_name }} (Self : in out Instance) return {{ connector.return_type }} is
{% if connector.return_type == "Sys_Time.T" and connector.kind == "get" %}
      -- Return the system time:
      To_Return : constant {{ connector.return_type }} := Self.System_Time;
{% else %}
      To_Return : {{ connector.return_type }};
{% endif %}
{% elif connector.kind == "request" %}
   overriding function {{ connector.tester_name }} (Self : in out Instance; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }} is
{% if connector.type == "Data_Product_Fetch.T" and connector.return_type == "Data_Product_Return.T" and data_dependencies %}
      To_Return : constant {{ connector.return_type }} := Self.Return_Data_Dependency (Arg);
{% else %}
      To_Return : {{ connector.return_type }};
{% endif %}
{% else %}
   overriding procedure {{ connector.tester_name }} (Self : in out Instance; Arg : {{ connector.mode }} {{ connector.type }}) is
{% endif %}
   begin
      -- Push the argument onto the test history for looking at later:
{% if connector.kind in ["get"] %}
      Self.{{ connector.tester_name }}_History.Push (To_Return);
{% else %}
      Self.{{ connector.tester_name }}_History.Push (Arg);
{% endif %}
{% if connector.type == "Event.T" and events   %}
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
{% endif %}
{% if connector.type == "Data_Product.T" and data_products %}
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
{% endif %}
{% if connector.type == "Packet.T" and packets %}
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
{% endif %}
{% if connector.type == "Fault.T" and faults %}
      -- Dispatch the fault to the correct handler:
      Self.Dispatch_Fault (Arg);
{% endif %}
{% if connector.kind in ["get", "request"] %}
      return To_Return;
{% endif %}
   end {{ connector.tester_name }};

{% endfor %}
{% endif %}
{% if connectors.of_kind("recv_async") %}
   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
{% for connector in connectors.of_kind("recv_async") %}
   -- This procedure is called when a {{ connector.tester_name }} message is dropped due to a full queue.
   overriding procedure {{ connector.tester_name }}_Dropped (Self : in out Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.tester_name }}_Index{% endif %}; Arg : in {{ connector.type }}) is
      Ignore : {{ connector.type }} renames Arg;
   begin
      if not Self.Expect_{{ connector.tester_name }}_Dropped then
{% if connector.count == 0 or connector.count > 1 %}
         pragma Assert (False, "The component's queue filled up when {{ connector.tester_name }} at index " & {{ connector.tester_name }}_Index'Image (Index) & " was called!");
{% else %}
         pragma Assert (False, "The component's queue filled up when {{ connector.tester_name }} was called!");
{% endif %}
      else
         Self.{{ connector.tester_name }}_Dropped_Count := @ + 1;
         Self.Expect_{{ connector.tester_name }}_Dropped := False;
      end if;
   end {{ connector.tester_name }}_Dropped;

{% endfor %}
{% endif %}
{% if events %}
   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
{% if events.description %}
   -- Description:
{{ printMultiLine(events.description, '   --    ') }}
{% endif %}
{% for event in events %}
{% if event.description %}
{{ printMultiLine(event.description, '   -- ') }}
{% endif %}
   overriding procedure {{ event.name }} (Self : in out Instance{% if event.type %}; Arg : in {{ event.type }}{% endif %}) is
{% if not event.type %}
      Arg : constant Natural := 0;
{% endif %}
   begin
      -- Push the argument onto the test history for looking at later:
      Self.{{ event.name }}_History.Push (Arg);
   end {{ event.name }};

{% endfor %}
{% endif %}
{% if data_products %}
   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
{% if data_products.description %}
   -- Description:
{{ printMultiLine(data_products.description, '   --    ') }}
{% endif %}
{% for dp in data_products %}
{% if dp.description %}
{{ printMultiLine(dp.description, '   -- ') }}
{% endif %}
   overriding procedure {{ dp.name }} (Self : in out Instance; Arg : in {{ dp.type }}) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.{{ dp.name }}_History.Push (Arg);
   end {{ dp.name }};

{% endfor %}
{% endif %}
{% if packets %}
   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
{% if packets.description %}
   -- Description:
{{ printMultiLine(packets.description, '   --    ') }}
{% endif %}
{% for p in packets %}
{% if p.description %}
{{ printMultiLine(p.description, '   -- ') }}
{% endif %}
{% if p.type %}
   overriding procedure {{ p.name }} (Self : in out Instance; Arg : in {{ p.type }}) is
{% else %}
   overriding procedure {{ p.name }} (Self : in out Instance; Arg : in Packet.T) is
{% endif %}
   begin
      -- Push the argument onto the test history for looking at later:
      Self.{{ p.name }}_History.Push (Arg);
   end {{ p.name }};

{% endfor %}
{% endif %}
{% if faults %}
   -----------------------------------------------
   -- Fault handler primitive:
   -----------------------------------------------
{% if faults.description %}
   -- Description:
{{ printMultiLine(faults.description, '   --    ') }}
{% endif %}
{% for fault in faults %}
{% if fault.description %}
{{ printMultiLine(fault.description, '   -- ') }}
{% endif %}
   overriding procedure {{ fault.name }} (Self : in out Instance{% if fault.type %}; Arg : in {{ fault.type }}{% endif %}) is
{% if not fault.type %}
      Arg : constant Natural := 0;
{% endif %}
   begin
      -- Push the argument onto the test history for looking at later:
      Self.{{ fault.name }}_History.Push (Arg);
   end {{ fault.name }};

{% endfor %}
{% endif %}
{% if connectors.of_kind("recv_async") %}
   -----------------------------------------------
   -- Special primitives for activating component
   -- queues:
   -----------------------------------------------
   -- Force the component to drain the entire queue
   not overriding function Dispatch_All (Self : in out Instance) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching all items off queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_All;
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_All;

   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching up to " & String_Util.Trim_Both (Positive'Image (N)) & " items from queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_N (N);
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_N;

{% endif %}
{% if execution in ["active", "either"] and not connectors.of_kind("recv_async") %}
   -----------------------------------------------
   -- Special primitives for running an active component
   -- with no queue:
   -----------------------------------------------
   -- Force the component to cycle from the tester
   not overriding procedure Cycle_Component (Self : in out Instance) is
   begin
      Self.Log ("    Beginning cycle of component.");
      -- Tell the component to cycle.
      Self.Component_Instance.Cycle;
      Self.Log ("    Cycle of component complete.");
   end Cycle_Component;

{% endif %}
{% if parameters %}
   -----------------------------------------------
   -- Special primitives for aiding in the staging,
   -- fetching, and updating of parameters
   -----------------------------------------------
   not overriding function Stage_Parameter (Self : in out Instance; Par : in Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      Param_Update : Parameter_Update.T := (
         Operation => Stage,
         Status => Success,
         Param => Par
      );
   begin
      Self.Parameter_Update_T_Provide (Param_Update);
      return Param_Update.Status;
   end Stage_Parameter;

   not overriding function Fetch_Parameter (Self : in out Instance; Id : in Parameter_Types.Parameter_Id; Par : out Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      Param_Update : Parameter_Update.T := (
         Operation => Fetch,
         Status => Success,
         Param => (Header => (Id => Id, Buffer_Length => 0), Buffer => (others => 0))
      );
   begin
      -- Set the ID to fetch:
      Param_Update.Param.Header.Id := Id;
      Self.Parameter_Update_T_Provide (Param_Update);
      Par := Param_Update.Param;
      return Param_Update.Status;
   end Fetch_Parameter;

   not overriding function Update_Parameters (Self : in out Instance) return Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      Param_Update : Parameter_Update.T := (
         Operation => Update,
         Status => Success,
         Param => ((0, 0), (others => 0))
      );
   begin
      Self.Parameter_Update_T_Provide (Param_Update);
      return Param_Update.Status;
   end Update_Parameters;

{% endif %}
end Component.{{ name }}.Implementation.Tester;
