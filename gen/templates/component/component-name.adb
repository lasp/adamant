--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Base Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Includes:
{% if connectors.n_arrayed().invoker() %}
with Safe_Deallocator;
{% endif %}
{% for include in base_adb_includes %}
with {{ include }};
{% endfor %}

package body Component.{{ name }} is

{% if parameters %}
   -- A protected object is used to store the component's staged parameters. This is because
   -- the staged parameters are accessed by both the execution thread of the component and the
   -- execution thread of the parameter component responsible for updating the parameters.
   -- The design of this protected object is to optimize the speed at which the copying of parameters
   -- from the staged to the working variables is as fast as possible.
   protected body Protected_Staged_Parameters is
      -- Sets the parameters staged flag to True
      procedure Set_Parameters_Staged is
      begin
         Parameters_Staged := True;
      end Set_Parameters_Staged;

      -- Returns true if the parameters have been staged:
      function Have_Parameters_Been_Staged return Boolean is
      begin
         return Parameters_Staged;
      end Have_Parameters_Been_Staged;

      -- Staging functions for each parameter:
{% for par in parameters %}
      procedure Stage_{{ par.name }} (Par : in {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %}) is
      begin
         {{ par.name }}_Staged := Par;
      end Stage_{{ par.name }};

{% endfor %}
{% for par in parameters %}
      function Get_{{ par.name }} return {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %} is
      begin
         return {{ par.name }}_Staged;
      end Get_{{ par.name }};

{% endfor %}
      -- Single update function to copy all the parameters from the
      -- staged versions to the working copy passed in. This function
      -- also resets the parameters_Updated boolean to False.
      procedure Copy_From_Staged (
{% for par in parameters %}
         {{ par.name }} : out {% if par.type_package %}{{ par.type_package }}.U{% else %}{{ par.type }}{% endif %}{{ ";" if not loop.last }}
{% endfor %}
      ) is
      begin
         if Parameters_Staged then
            -- Copy over all the parameters from the staged to the passed in values:
{% for par in parameters %}
            {{ par.name }} := {{ par.name }}_Staged;
{% endfor %}
            -- We have now updated all the parameters, so reset the staged flag:
            Parameters_Staged := False;
         end if;
      end Copy_From_Staged;
   end Protected_Staged_Parameters;

{% endif %}
{% if connectors.requires_queue() %}
   ---------------------------------------
   -- Return max queue size element:
   ---------------------------------------
   not overriding function Get_Max_Queue_Element_Size (Self : in Base_Instance) return Natural is
      Ignore : Base_Instance renames Self;
   begin
      return Max_Queue_Element_Size;
   end Get_Max_Queue_Element_Size;

   overriding function Get_Queue_Current_Percent_Used (Self : in out Base_Instance) return Basic_Types.Byte is
   begin
      return Self.Queue.Current_Percent_Used;
   end Get_Queue_Current_Percent_Used;

   overriding function Get_Queue_Maximum_Percent_Used (Self : in out Base_Instance) return Basic_Types.Byte is
   begin
      return Self.Queue.Max_Percent_Used;
   end Get_Queue_Maximum_Percent_Used;

{% endif %}
{% if init_base %}
   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
{% if init_base.description %}
{{ printMultiLine(init_base.description, '   -- ') }}
{% endif %}
{% if init_base.parameters %}
   --
{{ printMultiLine("Init Base Parameters:", '   -- ') }}
{% for p in init_base.parameters %}
{{ printMultiLine(p.name + " : " + p.type + ((" - " + p.description) if (p.description) else ""), '   -- ') }}
{% endfor %}
   --
{% endif %}
   not overriding procedure Init_Base (Self : in out Base_Instance{% if init_base.parameters %}; {{ init_base.parameter_declaration_string() }}{% endif %}) is
   begin
{% if connectors.requires_priority_queue() %}
      Self.Queue.Init (Element_Size => Max_Queue_Element_Size, Depth => Priority_Queue_Depth);
{% elif connectors.requires_queue() %}
      Self.Queue.Init (Queue_Size);
{% endif %}
{% if connectors.n_arrayed().invoker() %}
{% for connector in connectors.n_arrayed().invoker() %}
      -- Allocated the arrayed connector for the invoker connector {{ connector.name }}:
      if {{ connector.name }}_Count > 0 then
         Self.Connector_{{ connector.name }} := new {{ connector.name }}_Array ({{ connector.name }}_Index'First .. {{ connector.name }}_Index'First + {{ connector.name }}_Count - 1);
      end if;
{% endfor %}
{% endif %}
{% if connectors.n_arrayed().invokee() %}
{% for connector in connectors.n_arrayed().invokee() %}
      -- Save the length for the arrayed connector for the invokee connector {{ connector.name }}:
      Self.{{ connector.name }}_Count := {{ connector.name }}_Count;
{% endfor %}
{% endif %}
   end Init_Base;
{% if connectors.of_kind("recv_async") or connectors.n_arrayed().invoker() %}

   not overriding procedure Final_Base (Self : in out Base_Instance) is
{% if connectors.n_arrayed().invoker() %}
{% for connector in connectors.n_arrayed().invoker() %}
      procedure Free_{{ connector.name }}_Array is new Safe_Deallocator.Deallocate_If_Testing (
         Object => {{ connector.name }}_Array,
         Name => {{ connector.name }}_Array_Access
      );
{% endfor %}
{% endif %}
   begin
{% if connectors.requires_queue() %}
      Self.Queue.Destroy;
{% endif %}
{% if connectors.n_arrayed().invoker() %}
{% for connector in connectors.n_arrayed().invoker() %}
      if Self.Connector_{{ connector.name }} /= null then
         Free_{{ connector.name }}_Array (Self.Connector_{{ connector.name }});
      end if;
{% endfor %}
{% endif %}
   end Final_Base;
{% endif %}

{% endif %}
{% if set_id_bases %}
   -----------------------------------------------------------------------
   -- Initialize the id_Bases for any commands, data products, or events:
   -----------------------------------------------------------------------
{% if set_id_bases.description %}
{{ printMultiLine(set_id_bases.description, '   -- ') }}
{% endif %}
{% if set_id_bases.parameters %}
   --
{{ printMultiLine("Set Id Bases Parameters:", '   -- ') }}
{% for p in set_id_bases.parameters %}
{{ printMultiLine(p.name + " : " + p.type + ((" - " + p.description) if (p.description) else ""), '   -- ') }}
{% endfor %}
   --
{% endif %}
   not overriding procedure Set_Id_Bases (Self : in out Base_Instance{% if set_id_bases.parameters %}; {{ set_id_bases.parameter_declaration_string() }}{% endif %}) is
   begin
{% if events %}
      Self.Events.Set_Id_Base (Event_Id_Base);
{% endif %}
{% if data_products %}
      Self.Data_Products.Set_Id_Base (Data_Product_Id_Base);
{% endif %}
{% if packets and not packets.ids %}
      Self.Packets.Set_Id_Base (Packet_Id_Base);
{% endif %}
{% if faults and not faults.ids %}
      Self.Faults.Set_Id_Base (Fault_Id_Base);
{% endif %}
{% if commands %}
{% if (commands|length) > 1 %}
      -- ID base set too high for command ID set. This is checked in the assembly model.
      pragma Assert (Natural (Command_Id_Base) + {{ (commands|length) - 1 }} <= Natural (Command_Types.Command_Id'Last));
{% endif %}
      Self.Command_Id_Base := Command_Id_Base;
{% endif %}
{% if parameters %}
{% if (parameters|length) > 1 %}
      -- ID base set too high for parameter ID set. This is checked in the assembly model.
      pragma Assert (Natural (Parameter_Id_Base) + {{ (parameters|length) - 1 }} <= Natural (Parameter_Types.Parameter_Id'Last));
{% endif %}
      Self.Parameter_Id_Base := Parameter_Id_Base;
{% endif %}
   end Set_Id_Bases;

{% endif %}
{% if map_data_dependencies %}
   -----------------------------------------------------------------------
   -- Initialize the IDs for the component's data dependencies
   -----------------------------------------------------------------------
{% if map_data_dependencies.description %}
{{ printMultiLine(map_data_dependencies.description, '   -- ') }}
{% endif %}
{% if map_data_dependencies.parameters %}
   --
{{ printMultiLine("Set Data Dependency Ids Parameters:", '   -- ') }}
{% for p in map_data_dependencies.parameters %}
{{ printMultiLine(p.name + " : " + p.type + ((" - " + p.description) if (p.description) else ""), '   -- ') }}
{% endfor %}
   --
{% endif %}
   not overriding procedure Map_Data_Dependencies (Self : in out Base_Instance{% if map_data_dependencies.parameters %}; {{ map_data_dependencies.parameter_declaration_string() }}{% endif %}) is
   begin
      Self.Data_Dependencies.Set_Ids_And_Limits (
{% for dd in data_dependencies %}
         {{ dd.name }}_Id => {{ dd.name }}_Id,
         {{ dd.name }}_Stale_Limit => {{ dd.name }}_Stale_Limit{{ "," if not loop.last }}
{% endfor %}
      );
   end Map_Data_Dependencies;

{% endif %}
{% if connectors.requires_queue() %}
   ---------------------------------------------------------------
   -- Visible private dispatching procedures for component queue:
   ---------------------------------------------------------------
   procedure Dispatch_Block (Self : in out Base_Instance) is
      use Queue_Package;
{% if connectors.requires_priority_queue() %}
      Pri_Element : Priority_Element;
{% else %}
      Id_Record : Connector_Identifier_Type;
{% endif %}
      Length : Natural;
      Bytes : Queue_Byte_Array;
   begin
      -- Pop an element off the queue and dispatch to the appropriate handler function
      -- based on the element type. Note: This call blocks if there is nothing on the
      -- queue.
{% if connectors.requires_priority_queue() %}
      case Self.Queue.Pop_Block (Priority => Pri_Element, Bytes => Bytes, Length => Length) is
{% else %}
      case Self.Queue.Pop_Block (Label => Id_Record, Bytes => Bytes, Length => Length) is
{% endif %}
         -- Dispatch message from queue to proper handler:
         -- The dispatch function MUST release the lock when finished.
         when Success =>
            declare
{% if connectors.requires_priority_queue() %}
               Id_Record : Connector_Identifier_Type renames Pri_Element.Id_Record;
{% endif %}
               Dispatch_To : constant Dispatch_Procedure := Dispatch_Table (Id_Record.Id);
            begin
               Dispatch_To (Self{% if connectors.arrayed_invokee() %}, Id_Record.Index{% endif %}, Bytes (Bytes'First .. Bytes'First + Length - 1));
               pragma Annotate (CodePeer, False_Positive, "range check", "We never put items on queue larger than Bytes'Length.");
            end;
         -- Error was returned:
         when Error =>
            -- Error was returned. This can only occur if another task was also waiting on this
            -- queue. In practice, this should always be avoided, as it goes against Adamant
            -- design practices. So, in practice the code below should never get executed.
            -- However, to be safe, let's just sleep for a short amount of time
            -- to give up the processor so we don't end up hogging the CPU if this is a high priority
            -- task, and then we can try to pop again when this function is called next in Cycle().
            --
            -- Note: This cannot happen on a SFP runtime, the last chance handler will get called instead.
            Sleep.Sleep_Ms (100);
{% if connectors.requires_priority_queue() %}
         -- Unexpected status was returned:
         when Too_Small =>
            -- The buffer should never be to small since it is sized to match the largest possible queue element size.
            pragma Assert (False);
{% endif %}
      end case;
   end Dispatch_Block;

   function Dispatch_Nonblock (Self : in out Base_Instance) return Boolean is
      use Queue_Package;
{% if connectors.requires_priority_queue() %}
      Pri_Element : Priority_Element;
{% else %}
      Id_Record : Connector_Identifier_Type;
{% endif %}
      Length : Natural;
      Bytes : Queue_Byte_Array;
   begin
      -- Pop an element off the queue and dispatch to the appropriate handler function
      -- based on the element type. Note: This call returns False if there is nothing
      -- on the queue:
{% if connectors.requires_priority_queue() %}
      case Self.Queue.Pop (Priority => Pri_Element, Bytes => Bytes, Length => Length) is
{% else %}
      case Self.Queue.Pop (Label => Id_Record, Bytes => Bytes, Length => Length) is
{% endif %}
         when Success =>
            declare
{% if connectors.requires_priority_queue() %}
               Id_Record : Connector_Identifier_Type renames Pri_Element.Id_Record;
{% endif %}
               Dispatch_To : constant Dispatch_Procedure := Dispatch_Table (Id_Record.Id);
            begin
               Dispatch_To (Self{% if connectors.arrayed_invokee() %}, Id_Record.Index{% endif %}, Bytes (Bytes'First .. Bytes'First + Length - 1));
               pragma Annotate (CodePeer, False_Positive, "range check", "We never put items on queue larger than Bytes'Length.");
            end;
            -- The dispatch function MUST release the lock when finished.
            return True;
         when Empty =>
            return False;
{% if connectors.requires_priority_queue() %}
         -- Unexpected status was returned:
         when Too_Small =>
            -- The buffer should never be to small since it is sized to match the largest possible queue element size.
            pragma Assert (False);
            return False;
{% endif %}
      end case;
   end Dispatch_Nonblock;

   not overriding function Dispatch_N (Self : in out Base_Instance; N : in Natural := 1) return Natural is
      Elements_Dispatched : Natural := 0;
   begin
      while Elements_Dispatched < N loop
         if Base_Instance'Class (Self).Dispatch_Nonblock = False then
            return Elements_Dispatched;
         end if;
         Elements_Dispatched := Elements_Dispatched + 1;
      end loop;
      return Elements_Dispatched;
   end Dispatch_N;

   not overriding function Dispatch_All (Self : in out Base_Instance) return Natural is
   begin
      return Self.Dispatch_N (Self.Queue.Num_Elements);
   end Dispatch_All;

   not overriding procedure Dispatch_Quit (Ignore : in out Base_Instance;{% if connectors.arrayed_invokee() %} Ignore_Idx : in Connector_Index_Type;{% endif %} Ignore_Bytes : in Basic_Types.Byte_Array) is
   begin
      null;
   end Dispatch_Quit;

{% for connector in connectors.of_kind('recv_async') %}
   not overriding function Enqueue_{{ connector.name }} (Self : in out Base_Instance{% if connectors.arrayed_invokee() %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : in {{ connector.type }}; Full_Queue_Behavior : in Full_Queue_Action := Drop) return Connector_Types.Connector_Status is
      use Queue_Package;
{% if connector.type_model %}
{% if connector.type_model.variable_length %}
      function Typed_Push is new Queue_Package.Push_Variable_Length_Type ({{ connector.type }}, {{ connector.type_package }}.Serialized_Length);
      function Typed_Push_Block is new Queue_Package.Push_Variable_Length_Type_Block ({{ connector.type }}, {{ connector.type_package }}.Serialized_Length);
{% else %}
      function Typed_Push is new Queue_Package.Push_Type ({{ connector.type }});
      function Typed_Push_Block is new Queue_Package.Push_Type_Block ({{ connector.type }});
{% endif %}
{% else %}
{% if connector.generic and connector.generic_serialized_length_func %}
      function Typed_Push is new Queue_Package.Push_Variable_Length_Type ({{ connector.type }}, {{ connector.generic_serialized_length_func.name }});
      function Typed_Push_Block is new Queue_Package.Push_Variable_Length_Type_Block ({{ connector.type }}, {{ connector.generic_serialized_length_func.name }});
{% else %}
      function Typed_Push is new Queue_Package.Push_Type ({{ connector.type }});
      function Typed_Push_Block is new Queue_Package.Push_Type_Block ({{ connector.type }});
{% endif %}
{% endif %}
      -- Form the meta data to put on the queue
      Id_Record : constant Connector_Identifier_Type := (Id => {{ connector.name }}{% if connectors.arrayed_invokee() %}, Index => Index{% endif %});
   begin
      -- Enqueue the message differently based on the full queue behavior:
      case Full_Queue_Behavior is
         -- Drop message if queue full:
         when Drop =>
{% if connectors.requires_priority_queue() %}
            case Typed_Push (Self.Queue, (Priority => {{ connector.priority }}, Id_Record => Id_Record), Arg) is
{% else %}
            case Typed_Push (Self.Queue, Id_Record, Arg) is
{% endif %}
               -- Push succeeded, nothing more to do here.
               when Success => null;
{% if (connector.type_model and connector.type_model.variable_length) or (connector.generic and connector.generic_serialized_length_func) %}
               -- Push failed due to serialization problem, return to caller.
               when Serialization_Failure =>
                  return Message_Dropped;
                  pragma Annotate (CodePeer, False_Positive, "dead code",
                     "Some types can never fail to serialize, thus this code will never be executed.");
{% endif %}
{% if connectors.requires_priority_queue() %}
               -- Push failed, return to caller.
               when Full => return Message_Dropped;
               -- Push too large, this shouldn't ever happen.
               when Too_Large =>
                  -- The buffer should never be to large the queue elements are sized to match the largest possible type.
                  pragma Assert (False);
{% else %}
               -- Push failed, return to caller.
               when Too_Full => return Message_Dropped;
{% endif %}
            end case;

         -- Block on full queue until it is not full anymore:
         when Wait =>
            declare
               Count : Natural := 0;
{% if (connector.type_model and connector.type_model.variable_length) or (connector.generic and connector.generic_serialized_length_func) %}
               Push_Stat : Push_Variable_Length_Type_Block_Status;
{% else %}
               Push_Stat : Push_Block_Status;
{% endif %}
            begin
{% if connectors.requires_priority_queue() %}
               Push_Stat := Typed_Push_Block (Self.Queue, (Priority => {{ connector.priority }}, Id_Record => Id_Record), Arg);
{% else %}
               Push_Stat := Typed_Push_Block (Self.Queue, Id_Record, Arg);
{% endif %}
               while Push_Stat = Error loop
                  -- Error was returned. This can only occur if another task was also waiting on this
                  -- queue. In practice, this should always be avoided, as it goes against Adamant
                  -- design practices. So, in practice the code below should never get executed.
                  -- However, to be safe, let's just sleep for a short amount of time
                  -- to give up the processor so we don't end up hogging the CPU if this is a high priority
                  -- task.
                  Count := Count + 1;
                  if Count > 5 then
                     -- We are still in an error condition after trying many time, something is terribly
                     -- wrong, drop the message.
                     return Message_Dropped;
                  else
                     Sleep.Sleep_Ms (100);
                  end if;
                  -- Try to push again.
{% if connectors.requires_priority_queue() %}
                  Push_Stat := Typed_Push_Block (Self.Queue, (Priority => {{ connector.priority }}, Id_Record => Id_Record), Arg);
{% else %}
                  Push_Stat := Typed_Push_Block (Self.Queue, Id_Record, Arg);
{% endif %}
               end loop;

               -- Check the status:
               case Push_Stat is
                  when Success =>
                     null;
                  when Error =>
                     -- This code is unreachable.
                     pragma Assert (False);
{% if (connector.type_model and connector.type_model.variable_length) or (connector.generic and connector.generic_serialized_length_func) %}
                  when Serialization_Failure =>
                     return Message_Dropped;
{% endif %}
{% if connectors.requires_priority_queue() %}
                  when Too_Large =>
                     -- The buffer should never be to large the queue elements are sized to match the largest possible type.
                     pragma Assert (False);
{% endif %}
               end case;

            end;
      end case;

      return Success;
   end Enqueue_{{ connector.name }};

{% endfor %}
{% endif %}
   ---------------------------------------------------------------
   -- Visible public Cycle function which defines task execution:
   ---------------------------------------------------------------
{% if execution in ["active", "either"] %}
{% if connectors.requires_queue() %}
   overriding procedure Cycle (Self : in out Base_Instance) is
   begin
      Base_Instance'Class (Self).Dispatch_Block;
   end Cycle;

   not overriding procedure Stop_Task (Self : in out Base_Instance) is
      use Queue_Package;
      Quit_Id_Record : constant Connector_Identifier_Type := (Id => Quit{% if connectors.arrayed_invokee() %}, Index => Connector_Index_Type'First{% endif %});
      Bytes : Basic_Types.Byte_Array (1 .. 0); -- Empty byte array
   begin
{% if connectors.requires_priority_queue() %}
      while Self.Queue.Push_Block ((Priority => Queue_Priority_Type'Last, Id_Record => Quit_Id_Record), Bytes) /= Success loop
{% else %}
      while Self.Queue.Push_Block (Quit_Id_Record, Bytes) /= Success loop
{% endif %}
         -- Error was returned. This can only occur if another task was also waiting on this
         -- queue. In practice, this should always be avoided, as it goes against Adamant
         -- design practices. So, in practice the code below should never get executed.
         -- However, to be safe, let's just sleep for a short amount of time
         -- to give up the processor so we don't end up hogging the CPU if this is a high priority
         -- task, and then we can try to push the exit again.
         --
         -- Note: This cannot happen on a SFP runtime, the last chance handler will get called instead.
         Sleep.Sleep_Ms (100);
      end loop;
   end Stop_Task;
{% else %}
   -- "Cycle" method is abstract, and should be overridden in implementation.
{% endif %}
{% else %}
   overriding procedure Cycle (Self : in out Base_Instance) is
      pragma Annotate (CodePeer, Intentional, "subp always fails",
         "Intentional - this subp should never be called on a component without a task.");
      Ignore : Base_Instance renames Self;
   begin
      -- This is a passive component, meaning it CANNOT be tasked. If the component
      -- is given a task we quit.
      --
      pragma Assert (False);
   end Cycle;
{% endif %}

{% if tasks.has_subtasks %}
   -------------------------------------------------------
   -- Definition of subtasks:
   -------------------------------------------------------
{% for subtask in tasks.subtask_list %}
   task body {{ subtask.name }}_Task is
   begin
      -- Initialize the stack's task_data and stack.
      Task_Util.Initialize_Task (Task_Data, Pri, Stack_Size, Secondary_Stack_Size);

      -- Wait to start until signaled:
      Ada.Synchronous_Task_Control.Suspend_Until_True (Signal.all);
      while not Ada.Synchronous_Task_Control.Current_State (Signal.all) loop
         -- Call the task procedure:
         Class_Self.all.{{ subtask.name }};
         -- Update secondary stack usage:
         Task_Util.Update_Secondary_Stack_Usage (Task_Data);
      end loop;
   end {{ subtask.name }}_Task;

{% endfor %}
{% endif %}
{% if connectors.invokee() %}
   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
{% for connector in connectors.invokee() %}
   not overriding function {{ connector.name }}_Access (Self : in Base_Instance; Index : in Connector_Index_Type{% if connector.count == 1 %} := Connector_Index_Type'First{% endif %}) return not null {{ connector.name }}_Connector.Invokee_Hook is
      -- Define index type so we can make sure invokee index is valid for the component.
{% if connector.count == 0 %}
      subtype Index_Type is {{ connector.name }}_Index range {{ connector.name }}_Index'First .. {{ connector.name }}_Index'First + Self.{{ connector.name }}_Count - 1;
{% else %}
      Ignore : Base_Instance renames Self;
      subtype Index_Type is {{ connector.name }}_Index;
{% endif %}
   begin
      -- Make sure the invokee index is within range. Index must be between Index_Type'First and Index_Type'Last.
{% if connector.count == 0 or connector.count > 1 %}
      pragma Assert (Index <= Index_Type'Last);
{% else %}
      pragma Assert (Index = Index_Type'Last and then Index = Index_Type'First);
{% endif %}
      return {{ connector.name }}_Hook'Access;
   end {{ connector.name }}_Access;

{% if connector.kind == "return" %}
   function {{ connector.name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Index : in Connector_Index_Type := Connector_Index_Type'First) return {{ connector.return_type }} is
{% elif connector.kind == "service" %}
   function {{ connector.name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First) return {{ connector.return_type }} is
{% elif connector.kind == "modify" %}
   procedure {{ connector.name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First) is
{% else %}
   function {{ connector.name }}_Hook (Class_Self : in out Component.Core_Instance'Class; Arg : {{ connector.mode }} {{ connector.type }}; Index : in Connector_Index_Type := Connector_Index_Type'First; Full_Queue_Behavior : in Full_Queue_Action := Drop) return Connector_Types.Connector_Status is
{% if connector.kind != "recv_async" %}
      -- This is a synchronous connector, so ignore the queue behavior.
      Ignore : Full_Queue_Action renames Full_Queue_Behavior;
{% endif %}
{% endif %}
      -- Convert the class instance to instance.
      Self : Base_Instance renames Base_Instance (Class_Self);
{% if connector.count == 0 %}
      -- Convert the index to the correct connector specific index type. If this type
      -- conversion fails, then there is a bug in the Adamant modeling system.
      subtype Constrained_{{ connector.name }}_Index is {{ connector.name }}_Index range {{ connector.name }}_Index'First .. {{ connector.name }}_Index'First + Self.{{ connector.name }}_Count - 1;
      Idx : constant Constrained_{{ connector.name }}_Index := Constrained_{{ connector.name }}_Index (Index);
{% elif connector.count > 1 or (connector.kind == "recv_async" and connectors.arrayed_invokee()) %}
      -- Convert the index to the correct connector specific index type. If this type
      -- conversion fails, then there is a bug in the Adamant modeling system.
      Idx : constant {{ connector.name }}_Index := {{ connector.name }}_Index (Index);
{% else %}
      -- This is not an arrayed connector, ignore the index:
      Ignore_Index : Connector_Index_Type renames Index;
{% endif %}
   begin
{% if connector.kind == "recv_async" %}
      -- Enqueue the message differently based on the full queue behavior:
      case Self.Enqueue_{{ connector.name }} ({% if connectors.arrayed_invokee() %}Idx, {% endif %}Arg, Full_Queue_Behavior) is
         -- Enqueue succeeded, nothing more to do here.
         when Success => null;
         when Message_Dropped =>
            -- Call up to the message dropped handler:
            Base_Instance'Class (Self).{{ connector.name }}_Dropped ({% if connector.count == 0 or connector.count > 1 %}Idx, {% endif %}Arg);
            return Message_Dropped;
      end case;
      return Success;
{% elif connector.kind == "return" %}
      -- Call up to the derived class' for execution:
      return Base_Instance'Class (Self).{{ connector.name }}{% if connector.count == 0 or connector.count > 1 %} (Idx){% endif %};
{% elif connector.kind == "service" %}
      -- Call up to the derived class' for execution:
      return Base_Instance'Class (Self).{{ connector.name }} ({% if connector.count == 0 or connector.count > 1 %}Idx, {% endif %}Arg);
{% elif connector.kind == "modify" %}
      -- Call up to the derived class' for execution:
      Base_Instance'Class (Self).{{ connector.name }} ({% if connector.count == 0 or connector.count > 1 %}Idx, {% endif %}Arg);
{% else %}
      -- Call up to the derived class' for execution:
      Base_Instance'Class (Self).{{ connector.name }} ({% if connector.count == 0 or connector.count > 1 %}Idx, {% endif %}Arg);
      return Success;
{% endif %}
   end {{ connector.name }}_Hook;

{% if connector.kind == "recv_async" %}
   not overriding procedure Dispatch_{{ connector.name }} (Self : in out Base_Instance;{% if connectors.arrayed_invokee() %} Index : in Connector_Index_Type;{% endif %} Bytes : in Basic_Types.Byte_Array) is
{% if connector.type_model %}
{% if not connector.type_model.variable_length %}
      package Type_Deserializer renames {{ connector.type_package }}.Serialization;
{% endif %}
      -- Optimization: Convert the incoming bytes to the proper type via overlay. This avoids
      -- an extra copy of the bytes:
      pragma Warnings (Off, "overlay changes scalar storage order");
      The_Type : {{ connector.type }} with Import, Convention => Ada, Address => Bytes'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
{% else %}
{% if connector.generic and connector.generic_serialized_length_func %}
      -- Optimization: Convert the incoming bytes to the proper type via overlay. This avoids
      -- an extra copy of the bytes:
      pragma Warnings (Off, "overlay changes scalar storage order");
      The_Type : {{ connector.type }} with Import, Convention => Ada, Address => Bytes'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
{% else %}
      package Type_Deserializer is new Serializer ({{ connector.type }});
      -- We use the serializer (requires copy) here instead of an overlay since there is sometimes alignment
      -- issues on embedded targets when using unpacked types and overlaying with a byte array.
      The_Type : constant {{ connector.type }} := Type_Deserializer.From_Byte_Array (Bytes);
{% endif %}
{% endif %}
{% if connector.count == 0 %}
      -- Convert the index to the correct connector specific index type. If this type
      -- conversion fails, then there is a bug, since this was already checked an enqueue.
      subtype Constrained_{{ connector.name }}_Index is {{ connector.name }}_Index range {{ connector.name }}_Index'First .. {{ connector.name }}_Index'First + Self.{{ connector.name }}_Count - 1;
      Idx : constant Constrained_{{ connector.name }}_Index := Constrained_{{ connector.name }}_Index (Index);
{% elif connector.count > 1 %}
      -- Convert the index to the correct connector specific index type. If this type
      -- conversion fails, then there is a bug, since this was already checked an enqueue.
      Idx : constant {{ connector.name }}_Index := {{ connector.name }}_Index (Index);
{% endif %}
   begin
{% if (connector.type_model and connector.type_model.variable_length) or (connector.generic and connector.generic_serialized_length_func) %}
      -- Special checks for a variable length serialized type:
      declare
         use Serializer_Types;
         Num_Bytes_Deserialized : Natural;
{% if connector.generic and connector.generic_serialized_length_func %}
         Ser_Status : constant Serialization_Status := {{ connector.generic_serialized_length_func.name }} (The_Type, Num_Bytes_Deserialized);
{% else %}
         Ser_Status : constant Serialization_Status := {{ connector.type_package }}.Serialized_Length (The_Type, Num_Bytes_Deserialized);
{% endif %}
      begin
         -- This type we serialized successfully, it should always deserialize successfully.
         pragma Assert (Ser_Status = Success);
         pragma Annotate (CodePeer, False_Positive, "assertion", "Assertion will only fail in case of data corruption of software bug.");
         -- The calculated length of the type should always equal the number of bytes received in the function.
         pragma Assert (Bytes'Length = Num_Bytes_Deserialized);
      end;
{% else %}
      -- Ensure the incoming bytes array is of the expected length. The length of the bytes should be the length of the serialized type.
      pragma Assert (Bytes'Length = Type_Deserializer.Serialized_Length);
{% endif %}

      -- Call up to the derived class' for execution.
      Base_Instance'Class (Self).{{ connector.name }} ({% if connector.count == 0 or connector.count > 1 %}Idx, {% endif %}The_Type);
   end Dispatch_{{ connector.name }};

{% endif %}
{% endfor %}
{% endif %}
{% if connectors.invoker() %}
   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
{% for connector in connectors.invoker() %}
   not overriding procedure Attach_{{ connector.name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; From_Index : in {{ connector.name }}_Index{% endif %}; To_Component : in not null Component.Class_Access; Hook : in not null {{ connector.name }}_Connector.Invokee_Hook; To_Index : in Connector_Index_Type := Connector_Index_Type'First) is
   begin
      Self.Connector_{{ connector.name }}{% if connector.count == 0 or connector.count > 1 %} (From_Index){% endif %}.Attach (To_Component, Hook, To_Index);
   end Attach_{{ connector.name }};

   not overriding function Is_{{ connector.name }}_Connected (Self : in Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}) return Boolean is
   begin
      return {{ connector.name }}_Connector.Is_Connected (Self.Connector_{{ connector.name }}{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %});
   end Is_{{ connector.name }}_Connected;

{% if connector.kind == "send" %}
   not overriding procedure {{ connector.name }} (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}; Full_Queue_Behavior : in Full_Queue_Action := Connector_Types.Drop) is
      Ret : Connector_Status;
   begin
      Ret := {{ connector.name }}_Connector.Call (Self.Connector_{{ connector.name }}{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %}, Arg, Full_Queue_Behavior);
      case Ret is
         -- Send worked fine, just continue on.
         when Success => null;
         -- The message was dropped. Call up to the message dropped handler:
         when Message_Dropped => {{ connector.name }}_Dropped (Base_Instance'Class (Self){% if connector.count == 0 or connector.count > 1 %}, Index{% endif %}, Arg);
      end case;
   end {{ connector.name }};

   not overriding procedure {{ connector.name }}_If_Connected (Self : in out Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}; Full_Queue_Behavior : in Full_Queue_Action := Connector_Types.Drop) is
   begin
      if Self.Is_{{ connector.name }}_Connected{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %} then
         Self.{{ connector.name }} ({% if connector.count == 0 or connector.count > 1 %}Index, {% endif %}Arg, Full_Queue_Behavior);
      end if;
   end {{ connector.name }}_If_Connected;
{% elif connector.kind == "get" %}
   not overriding function {{ connector.name }} (Self : in Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}) return {{ connector.return_type }} is
   begin
      return {{ connector.name }}_Connector.Call (Self.Connector_{{ connector.name }}{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %});
   end {{ connector.name }};
{% elif connector.kind == "request" %}
   not overriding function {{ connector.name }} (Self : in Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) return {{ connector.return_type }} is
   begin
      return {{ connector.name }}_Connector.Call (Self.Connector_{{ connector.name }}{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %}, Arg);
   end {{ connector.name }};
{% elif connector.kind == "provide" %}
   not overriding procedure {{ connector.name }} (Self : in Base_Instance{% if connector.count == 0 or connector.count > 1 %}; Index : in {{ connector.name }}_Index{% endif %}; Arg : {{ connector.mode }} {{ connector.type }}) is
   begin
      {{ connector.name }}_Connector.Call (Self.Connector_{{ connector.name }}{% if connector.count == 0 or connector.count > 1 %} (Index){% endif %}, Arg);
   end {{ connector.name }};
{% endif %}

{% endfor %}
{% endif %}
{% if commands %}
   -------------------------------------------
   -- Command handling primitives:
   -------------------------------------------
   -- Dispatch command to correct execution handler:
   not overriding function Execute_Command (Self : in out Base_Instance; Cmd : in Command.T) return Command_Response_Status.E is
      use Command_Types;
      use Command_Response_Status;
   begin
      -- Handle the special registration command:
      if Cmd.Header.Id = 0 then
         Self.Execute_Register_Commands (Cmd);
         return Register;
      end if;

      -- If ID is within the valid range then execute the command, otherwise do error routine:
      declare
         Integer_Cmd_Id : constant Integer := Integer (Cmd.Header.Id) - Integer (Self.Command_Id_Base);
      begin
         if Integer_Cmd_Id >= {{ commands.name }}.Local_Command_Id_Type'Enum_Rep ({{ commands.name }}.Local_Command_Id_Type'First) and then
             Integer_Cmd_Id <= {{ commands.name }}.Local_Command_Id_Type'Enum_Rep ({{ commands.name }}.Local_Command_Id_Type'Last)
         then
            declare
               Local_Id : constant {{ commands.name }}.Local_Command_Id_Type := {{ commands.name }}.Local_Command_Id_Type'Val (Integer_Cmd_Id);
               Execute_To : constant Execute_Function := Command_Id_Table (Local_Id);
            begin
               return Execute_To (Self, Cmd);
            end;
         else
            -- Id is not valid for component, so call the invalid command handler and return the the error status:
            declare
               P_Type : Basic_Types.Poly_Type := (others => 0);
            begin
               -- Copy id into poly type:
               Byte_Array_Util.Safe_Right_Copy (P_Type, Command_Id.Serialization.To_Byte_Array ((Id => Cmd.Header.Id)));
               Base_Instance'Class (Self).Invalid_Command (Cmd, Unsigned_32'Last - 1, P_Type);
               return Id_Error;
            end;
         end if;
      end;
   end Execute_Command;

   not overriding procedure Execute_Register_Commands (Self : in out Base_Instance; Cmd : in Command.T) is
      use Command_Types;
   begin
      pragma Assert (Cmd.Header.Id = 0);
      -- Deserialize the arguments and call up to the derived class for execution.
      Base_Instance'Class (Self).Register_Commands (Command_Registration_Request.Serialization.From_Byte_Array (Cmd.Arg_Buffer (Cmd.Arg_Buffer'First .. Cmd.Arg_Buffer'First + Command_Registration_Request.Serialization.Serialized_Length - 1)));
   end Execute_Register_Commands;

   -- Default implementation of special Register Commands handler. Override if you need different behavior.
   not overriding procedure Register_Commands (Self : in out Base_Instance; Arg : in Command_Registration_Request.T) is
      use Command_Types;
      use Command_Response_Status;
   begin
      -- Set the registration id in the component:
      Self.Command_Reg_Id := Arg.Registration_Id;

      -- Register each command:
      for Command_Enum in {{ commands.name }}.Local_Command_Id_Type loop
         -- Send the registration.
         Self.Command_Response_T_Send_If_Connected (
            (
               Source_Id => 0, -- Set to zero so this command registration does not get forwarded as a command response.
               Registration_Id => Self.Command_Reg_Id,
               Command_Id => Self.Command_Id_Base + Command_Types.Command_Id ({{ commands.name }}.Local_Command_Id_Type'Enum_Rep (Command_Enum)),
               Status => Register
            ),
            -- Waiting would risk more than one task on a protected object which would violate Ravenscar, so drop instead and
            -- hope user sees the event and corrects the problem.
            Full_Queue_Behavior => Connector_Types.Drop
         );
         pragma Annotate (CodePeer, False_Positive, "range check",
            "The command ID cannot be out of range since range checking is done in Set_Id_Bases.");
         -- Sleep a bit, so as to not stress out the command router component's queue.
         Sleep.Sleep_Us (Configuration.Command_Registration_Delay);
      end loop;
   end Register_Commands;

   not overriding procedure Handle_Command_Length_Error (Self : in out Base_Instance; Cmd : in Command.T) is
      P_Type : Basic_Types.Poly_Type := (others => 0);
   begin
      -- Copy length into poly type:
      Byte_Array_Util.Safe_Right_Copy (P_Type, Command_Arg_Buffer_Length.Serialization.To_Byte_Array ((Arg_Buffer_Length => Cmd.Header.Arg_Buffer_Length)));
      -- Call up to the command_Invalid function for handling.
      Base_Instance'Class (Self).Invalid_Command (Cmd, Unsigned_32'Last, P_Type);
   end Handle_Command_Length_Error;

{% for command in commands %}
   not overriding function Execute_{{ command.name }} (Self : in out Base_Instance; Cmd : in Command.T) return Command_Response_Status.E is
      use Command_Types;
{% if command.type %}
{% if command.type_model %}
      package Arg_Deserializer renames {{ command.type_package }}.Serialization;
{% if command.type_model.variable_length %}
      use Serializer_Types;
      Stat : Serialization_Status;
      Num_Bytes_Deserialized : Natural;
      Args : {{ command.type }};
{% endif %}
{% else %}
      package Arg_Deserializer is new Serializer ({{ command.type }});
{% endif %}
{% endif %}
   begin
      pragma Assert (Cmd.Header.Id = Self.Command_Id_Base + {{ loop.index0 }});
      pragma Annotate (CodePeer, False_Positive, "assertion", "Internal routing ensures this is true.");

{% if command.type %}
{% if command.type_model and command.type_model.variable_length %}
      -- Deserialize the variable length command argument into its argument type. Make sure
      -- that the command length includes at least as many bytes as was deserialized, otherwise
      -- something is amiss. It is OK for the command length to include more bytes than are
      -- actually deserialized. This "padding" is simply ignored.
      Stat := Arg_Deserializer.From_Byte_Array (Args, Cmd.Arg_Buffer (Cmd.Arg_Buffer'First .. Cmd.Arg_Buffer'First + Cmd.Header.Arg_Buffer_Length - 1), Num_Bytes_Deserialized);
      if Stat = Success and then Cmd.Header.Arg_Buffer_Length >= Num_Bytes_Deserialized then
         pragma Annotate (CodePeer, Intentional, "condition predetermined", "Sometimes the length can never be too large based on its type, and that is just fine.");
         declare
{% else %}
      -- Check the command argument length and make sure it is valid.
      if Cmd.Header.Arg_Buffer_Length = Arg_Deserializer.Serialized_Length then
         declare
            -- Deserialize the command arguments into their argument type:
            Args : constant {{ command.type }} := Arg_Deserializer.From_Byte_Array (Cmd.Arg_Buffer (Cmd.Arg_Buffer'First .. Cmd.Arg_Buffer'First + Arg_Deserializer.Serialized_Length - 1));
{% endif %}
{% if command.type_model %}
            Errant_Field : Unsigned_32 := 0;
            pragma Annotate (CodePeer, Intentional, "unused assignment", "Sometimes the type can never be invalid, and in that case Errant_Field will never be needed.");
            Args_Valid : constant Boolean := {{ command.type_package }}.Validation.Valid (Args, Errant_Field);
{% else %}
            Errant_Field : constant Unsigned_32 := 0;
            Args_Valid : constant Boolean := Args'Valid;
{% endif %}
         begin
            -- Make sure the deserialized argument values are valid.
            if Args_Valid then
               -- Call up to the derived class for execution.
               declare
                  use Command_Execution_Status;
                  Cmd_Execution_Stat : constant Command_Execution_Status.E := Base_Instance'Class (Self).{{ command.name }} (Args);
               begin
                  case Cmd_Execution_Stat is
                     when Success =>
                        return Command_Response_Status.Success;
                     when Failure =>
                        return Command_Response_Status.Failure;
                  end case;
               end;
            else
               -- Create a poly type with the invalid parameter and send it to the handler.
               declare
{% if command.type_model %}
                  P_Type : constant Basic_Types.Poly_Type := {{ command.type_package }}.Validation.Get_Field (Args, Errant_Field);
{% else %}
                  P_Type : Basic_Types.Poly_Type := (others => 0);
{% endif %}
               begin
{% if not command.type_model %}
                  -- Copy args into poly type:
                  Byte_Array_Util.Safe_Right_Copy (P_Type, Cmd.Arg_Buffer (Cmd.Arg_Buffer'First .. Cmd.Arg_Buffer'First + Arg_Deserializer.Serialized_Length - 1));
{% endif %}
                  -- Call up to the command_Invalid function for handling.
                  Base_Instance'Class (Self).Invalid_Command (Cmd, Errant_Field, P_Type);
                  return Command_Response_Status.Validation_Error;
               end;
            end if;
         end;
      else
{% else %}
      -- Check the command argument length and make sure it is valid.
      if Cmd.Header.Arg_Buffer_Length = 0 then
         -- Call up to the derived class for execution.
         declare
            use Command_Execution_Status;
            Cmd_Execution_Stat : constant Command_Execution_Status.E := Base_Instance'Class (Self).{{ command.name }};
         begin
            case Cmd_Execution_Stat is
               when Success =>
                  return Command_Response_Status.Success;
               when Failure =>
                  return Command_Response_Status.Failure;
            end case;
         end;
      else
{% endif %}
         Self.Handle_Command_Length_Error (Cmd);
         return Command_Response_Status.Length_Error;
      end if;
   end Execute_{{ command.name }};

{% endfor %}
{% endif %}
{% if parameters %}
   -------------------------------------------
   -- Parameter handling primitives:
   -------------------------------------------

   not overriding procedure Update_Parameters (Self : in out Base_Instance) is
   begin
      -- If parameters have finished being staged, then we need to update
      -- our local working copy from the staged parameter store.
      if Self.Staged_Parameters.Have_Parameters_Been_Staged then
         -- Copy over staged parameters to working parameters:
         Self.Staged_Parameters.Copy_From_Staged (
{% for par in parameters %}
            {{ par.name }} => Self.{{ par.name }}{{ "," if not loop.last }}
{% endfor %}
         );
         -- Now that the parameters have been updated, call up to the user function to
         -- implement any special action that needs to be run with the updated parameters.
         Base_Instance'Class (Self).Update_Parameters_Action;
      end if;
   end Update_Parameters;

   not overriding procedure Process_Parameter_Update (Self : in out Base_Instance; Par_Update : in out Parameter_Update.T) is
      use Parameter_Operation_Type;
      Status : Parameter_Update_Status.E := Parameter_Update_Status.Success;
   begin
      case Par_Update.Operation is
         when Stage =>
            -- Stage this parameter.
            Status := Self.Stage_Parameter (Par_Update.Param);
         when Update =>
            -- All parameters have been staged, we can now update our local parameters:
            Self.Staged_Parameters.Set_Parameters_Staged;
         when Fetch =>
            Status := Self.Fetch_Parameter (Par_Update.Param);
      end case;

      -- Set the return status:
      Par_Update.Status := Status;
   end Process_Parameter_Update;

   not overriding function Stage_Parameter (Self : in out Base_Instance; Par : in Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Types;
      use Parameter_Update_Status;
   begin
      -- If ID is within the valid range then stage the parameter, otherwise do error routine:
      declare
         Integer_Param_Id : constant Integer := Integer (Par.Header.Id) - Integer (Self.Parameter_Id_Base);
      begin
         if Integer_Param_Id >= {{ parameters.name }}.Local_Parameter_Id_Type'Enum_Rep ({{ parameters.name }}.Local_Parameter_Id_Type'First) and then
             Integer_Param_Id <= {{ parameters.name }}.Local_Parameter_Id_Type'Enum_Rep ({{ parameters.name }}.Local_Parameter_Id_Type'Last)
         then
            declare
               Local_Id : constant {{ parameters.name }}.Local_Parameter_Id_Type := {{ parameters.name }}.Local_Parameter_Id_Type'Val (Par.Header.Id - Self.Parameter_Id_Base);
               Stage_To : constant Stage_Function := Parameter_Id_Table (Local_Id);
            begin
               return Stage_To (Self, Par);
            end;
         else
            -- Id is not valid for component, so call the invalid parameter handler and return the the error status:
            declare
               P_Type : Basic_Types.Poly_Type := (others => 0);
            begin
               -- Copy id into poly type:
               Byte_Array_Util.Safe_Right_Copy (P_Type, Parameter_Id.Serialization.To_Byte_Array ((Id => Par.Header.Id)));
               Base_Instance'Class (Self).Invalid_Parameter (Par, Unsigned_32'Last - 1, P_Type);
               return Id_Error;
            end;
         end if;
      end;
   end Stage_Parameter;

   not overriding procedure Handle_Parameter_Length_Error (Self : in out Base_Instance; Par : in Parameter.T) is
      P_Type : Basic_Types.Poly_Type := (others => 0);
   begin
      -- Copy length into poly type:
      Byte_Array_Util.Safe_Right_Copy (P_Type, Parameter_Buffer_Length.Serialization.To_Byte_Array ((Buffer_Length => Par.Header.Buffer_Length)));
      -- Call up to the parameter_Invalid function for handling.
      Base_Instance'Class (Self).Invalid_Parameter (Par, Unsigned_32'Last, P_Type);
   end Handle_Parameter_Length_Error;

{% for par in parameters %}
   not overriding function Stage_{{ par.name }} (Self : in out Base_Instance; Par : in Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Types;
{% if par.type_model %}
      package Buffer_Deserializer renames {{ par.type_package }}.Serialization;
{% else %}
      package Buffer_Deserializer is new Serializer ({{ par.type }});
{% endif %}
   begin
      pragma Assert (Par.Header.Id = Self.Parameter_Id_Base + {{ loop.index0 }});
      pragma Annotate (CodePeer, False_Positive, "assertion", "Internal routing ensures this is true.");

      -- Check the parameter buffer length and make sure it is valid.
      if Par.Header.Buffer_Length = Buffer_Deserializer.Serialized_Length then
         declare
            -- Deserialize the parameter buffer into the buffer type:
            Par_To_Stage : constant {{ par.type }} := Buffer_Deserializer.From_Byte_Array (Par.Buffer (Par.Buffer'First .. Par.Buffer'First + Buffer_Deserializer.Serialized_Length - 1));
{% if par.type_model %}
            Errant_Field : Unsigned_32 := 0;
            pragma Annotate (CodePeer, Intentional, "unused assignment", "Sometimes the type can never be invalid, and in that case Errant_Field will never be needed.");
            Args_Valid : constant Boolean := {{ par.type_package }}.Validation.Valid (Par_To_Stage, Errant_Field);
{% else %}
            Errant_Field : constant Unsigned_32 := 0;
            Args_Valid : constant Boolean := Par_To_Stage'Valid;
{% endif %}
         begin
            -- Make sure the deserialized parameter values are valid.
            if Args_Valid then
               -- Stage the parameter:
{% if par.type_package %}
               Self.Staged_Parameters.Stage_{{ par.name }} ({{ par.type_package }}.Unpack (Par_To_Stage));
{% else %}
               Self.Staged_Parameters.Stage_{{ par.name }} (Par_To_Stage);
{% endif %}
               return Parameter_Update_Status.Success;
            else
               -- Create a poly type with the invalid parameter and send it to the handler.
               declare
{% if par.type_model %}
                  P_Type : constant Basic_Types.Poly_Type := {{ par.type_package }}.Validation.Get_Field (Par_To_Stage, Errant_Field);
{% else %}
                  P_Type : Basic_Types.Poly_Type := (others => 0);
{% endif %}
               begin
{% if not par.type_model %}
                  -- Copy parameter value into poly type:
                  Byte_Array_Util.Safe_Right_Copy (P_Type, Par.Buffer (Par.Buffer'First .. Par.Buffer'First + Buffer_Deserializer.Serialized_Length - 1));
{% endif %}
                  -- Call up to the parameter_Invalid function for handling.
                  Base_Instance'Class (Self).Invalid_Parameter (Par, Errant_Field, P_Type);
                  return Parameter_Update_Status.Validation_Error;
               end;
            end if;
         end;
      else
         Self.Handle_Parameter_Length_Error (Par);
         return Parameter_Update_Status.Length_Error;
      end if;
   end Stage_{{ par.name }};

{% endfor %}
   not overriding function Fetch_Parameter (Self : in out Base_Instance; Par : in out Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Types;
      use Parameter_Update_Status;
   begin
      -- If ID is within the valid range then stage the parameter, otherwise do error routine:
      declare
         Integer_Param_Id : constant Integer := Integer (Par.Header.Id) - Integer (Self.Parameter_Id_Base);
      begin
         if Integer_Param_Id >= {{ parameters.name }}.Local_Parameter_Id_Type'Enum_Rep ({{ parameters.name }}.Local_Parameter_Id_Type'First) and then
             Integer_Param_Id <= {{ parameters.name }}.Local_Parameter_Id_Type'Enum_Rep ({{ parameters.name }}.Local_Parameter_Id_Type'Last)
         then
            declare
               Local_Id : constant {{ parameters.name }}.Local_Parameter_Id_Type := {{ parameters.name }}.Local_Parameter_Id_Type'Val (Par.Header.Id - Self.Parameter_Id_Base);
               Fetch_From : constant Fetch_Function := Parameter_Id_Fetch_Table (Local_Id);
            begin
               return Fetch_From (Self, Par);
            end;
         else
            -- Id is not valid for component so return the the error status:
            return Id_Error;
         end if;
      end;
   end Fetch_Parameter;

{% for par in parameters %}
   not overriding function Fetch_{{ par.name }} (Self : in out Base_Instance; Par : in out Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Types;
{% if par.type_model %}
      package Buffer_Deserializer renames {{ par.type_package }}.Serialization;
      Value : constant {{ par.type }} := {{ par.type_package }}.Pack (Self.Staged_Parameters.Get_{{ par.name }});
{% else %}
      package Buffer_Deserializer is new Serializer ({{ par.type }});
      Value : constant {{ par.type }} := Self.Staged_Parameters.Get_{{ par.name }};
{% endif %}
      pragma Annotate (CodePeer, False_Positive, "validity check",
         "Defaults for parameter values are always initialized within Staged_Parameters protected object definition.");
   begin
      pragma Assert (Par.Header.Id = Self.Parameter_Id_Base + {{ loop.index0 }});

      -- Set the parameter length:
      Par.Header.Buffer_Length := Buffer_Deserializer.Serialized_Length;

      -- Set the parameter buffer data:
      Par.Buffer (Par.Buffer'First .. Par.Buffer'First + Par.Header.Buffer_Length - 1) := Buffer_Deserializer.To_Byte_Array (Value);
      return Parameter_Update_Status.Success;
   end Fetch_{{ par.name }};

{% endfor %}
{% endif %}
{% if data_dependencies %}
   -------------------------------------------
   -- Data dependency helper primitives:
   -------------------------------------------

   -- Private helper functions to get data dependencies:
{% for dd in data_dependencies %}
   not overriding function Get_{{ dd.name }} (Self : in out Base_Instance; Stale_Reference : in Sys_Time.T; Timestamp : out Sys_Time.T; Value : out {{ dd.type }}) return Data_Product_Enums.Data_Dependency_Status.E is
      use Data_Product_Enums.Data_Dependency_Status;
      Id : constant Data_Product_Types.Data_Product_Id := Self.Data_Dependencies.Get_{{ dd.name }}_Id;
      Ret : constant Data_Product_Return.T := Base_Instance'Class (Self).Get_Data_Dependency (Id => Id);
   begin
      -- Check status:
      case Ret.The_Status is
         when Data_Product_Enums.Fetch_Status.Success =>
            null; -- Continue on.
         when Data_Product_Enums.Fetch_Status.Not_Available =>
            return Not_Available;
         when Data_Product_Enums.Fetch_Status.Id_Out_Of_Range =>
            Base_Instance'Class (Self).Invalid_Data_Dependency (Id, Ret);
            return Error;
      end case;

      declare
         -- Extract data product value:
         Data_Dependency_Stat : constant {{ name }}_Data_Dependencies.Status := Self.Data_Dependencies.Extract_{{ dd.name }} (
            Product => Ret.The_Data_Product,
            Stale_Reference => Stale_Reference,
            Timestamp => Timestamp,
            Item => Value
         );
      begin
         case Data_Dependency_Stat is
            when {{ name }}_Data_Dependencies.Success =>
               return Success;
            when {{ name }}_Data_Dependencies.Stale =>
               return Stale;
            when {{ name }}_Data_Dependencies.Id_Error =>
               Base_Instance'Class (Self).Invalid_Data_Dependency (Id, Ret);
               return Error;
            when {{ name }}_Data_Dependencies.Length_Error =>
               Base_Instance'Class (Self).Invalid_Data_Dependency (Id, Ret);
               return Error;
         end case;
      end;
   end Get_{{ dd.name }};

   not overriding function Get_{{ dd.name }} (Self : in out Base_Instance; Stale_Reference : in Sys_Time.T; Value : out {{ dd.type }}) return Data_Product_Enums.Data_Dependency_Status.E is
      Ignore : Sys_Time.T;
   begin
      return Self.Get_{{ dd.name }} (Stale_Reference, Ignore, Value);
   end Get_{{ dd.name }};

{% endfor %}
{% endif %}
end Component.{{ name }};
