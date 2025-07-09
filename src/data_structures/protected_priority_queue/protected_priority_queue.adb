package body Protected_Priority_Queue is

   protected body Protected_Priority_Queue_Wrapper is

      procedure Init (Element_Size : in Positive; Depth : in Positive) is
      begin
         Queue.Init (Element_Size => Element_Size, Depth => Depth);
      end Init;

      procedure Destroy is
      begin
         Queue.Destroy;
      end Destroy;

      procedure Clear is
      begin
         Queue.Clear;
      end Clear;

      function Get_Count return Natural is
      begin
         return Queue.Get_Count;
      end Get_Count;

      function Get_Max_Count return Natural is
      begin
         return Queue.Get_Max_Count;
      end Get_Max_Count;

      function Num_Bytes_Free return Natural is
      begin
         return Queue.Num_Bytes_Free;
      end Num_Bytes_Free;

      function Num_Bytes_Used return Natural is
      begin
         return Queue.Num_Bytes_Used;
      end Num_Bytes_Used;

      function Max_Num_Bytes_Used return Natural is
      begin
         return Queue.Max_Num_Bytes_Used;
      end Max_Num_Bytes_Used;

      function Num_Bytes_Total return Natural is
      begin
         return Queue.Num_Bytes_Total;
      end Num_Bytes_Total;

      function Current_Percent_Used return Basic_Types.Byte is
      begin
         return Queue.Current_Percent_Used;
      end Current_Percent_Used;

      function Max_Percent_Used return Basic_Types.Byte is
      begin
         return Queue.Max_Percent_Used;
      end Max_Percent_Used;

      function Peek_Length (Priority : out Priority_Type; Length : out Natural) return Peek_Status is
         use Priority_Queue_Package;
      begin
         case Queue.Peek (Priority => Priority, Num_Bytes_Returned => Length) is
            when Success =>
               return Success;
            when Empty =>
               return Empty;
         end case;
      end Peek_Length;

      procedure Push (Priority : in Priority_Type; Bytes : in Basic_Types.Byte_Array; Not_Empty : in out Ada.Synchronous_Task_Control.Suspension_Object; Status : out Push_Status) is
         use Priority_Queue_Package;
      begin
         case Queue.Push (Priority => Priority, Bytes => Bytes) is
            when Success =>
               -- Release the not_empty suspension object:
               Ada.Synchronous_Task_Control.Set_True (Not_Empty);
               Status := Success;
            when Full =>
               Status := Full;
            when Too_Large =>
               Status := Too_Large;
         end case;
      end Push;

      procedure Pop (Priority : out Priority_Type; Bytes : out Basic_Types.Byte_Array; Not_Full : in out Ada.Synchronous_Task_Control.Suspension_Object; Length : out Natural; Status : out Pop_Status) is
         pragma Annotate (GNATSAS, False_Positive, "validity check", "Priority out parameter initialized by Queue.Pop on success");
         use Priority_Queue_Package;
      begin
         case Queue.Pop (Priority => Priority, Num_Bytes_Returned => Length, Bytes => Bytes) is
            when Success =>
               -- Release the not_full suspension object:
               Ada.Synchronous_Task_Control.Set_True (Not_Full);
               Status := Success;
            when Empty =>
               Status := Empty;
            when Too_Small =>
               Status := Too_Small;
         end case;
      end Pop;
   end Protected_Priority_Queue_Wrapper;

   procedure Init (Self : in out Instance; Element_Size : in Positive; Depth : in Positive) is
   begin
      Self.Queue.Init (Element_Size => Element_Size, Depth => Depth);

      -- Initialize the suspension objects:
      Ada.Synchronous_Task_Control.Set_False (Self.Not_Empty);
      Ada.Synchronous_Task_Control.Set_True (Self.Not_Full);
   end Init;

   procedure Destroy (Self : in out Instance) is
   begin
      Self.Queue.Destroy;
   end Destroy;

   procedure Clear (Self : in out Instance) is
   begin
      Self.Queue.Clear;

      -- Initialize the suspension objects:
      Ada.Synchronous_Task_Control.Set_False (Self.Not_Empty);
      Ada.Synchronous_Task_Control.Set_True (Self.Not_Full);
   end Clear;

   function Push (Self : in out Instance; Priority : in Priority_Type; Bytes : in Basic_Types.Byte_Array) return Push_Status is
      Status : Push_Status;
   begin
      Self.Queue.Push (Priority, Bytes, Self.Not_Empty, Status);
      return Status;
   end Push;

   function Pop (Self : in out Instance; Priority : out Priority_Type; Bytes : out Basic_Types.Byte_Array; Length : out Natural) return Pop_Status is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "Priority out parameter initialized by Queue.Pop on success");
      Status : Pop_Status;
   begin
      Self.Queue.Pop (Priority, Bytes, Self.Not_Full, Length, Status);
      return Status;
   end Pop;

   function Peek_Length (Self : in out Instance; Priority : out Priority_Type; Length : out Natural) return Peek_Status is
   begin
      return Self.Queue.Peek_Length (Priority, Length);
   end Peek_Length;

   function Push_Block (Self : in out Instance; Priority : in Priority_Type; Bytes : in Basic_Types.Byte_Array) return Push_Block_Status is
      Status : Push_Status;
   begin
      Self.Queue.Push (Priority, Bytes, Self.Not_Empty, Status);
      while Status = Full loop
         -- Suspend until the queue is no longer full:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Full);
         Self.Queue.Push (Priority, Bytes, Self.Not_Empty, Status);
      end loop;

      case Status is
         when Success =>
            return Success;
         when Too_Large =>
            return Too_Large;
         when Full =>
            pragma Assert (False, "Should not have exited loop if this was the case.");
            return Error;
      end case;
   exception
      when Program_Error =>
         return Error;
   end Push_Block;

   function Pop_Block (Self : in out Instance; Priority : out Priority_Type; Bytes : out Basic_Types.Byte_Array; Length : out Natural) return Pop_Block_Status is
      Status : Pop_Status;
   begin
      Self.Queue.Pop (Priority, Bytes, Self.Not_Full, Length, Status);
      while Status = Empty loop
         -- Suspend until the queue is no longer empty:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Empty);
         Self.Queue.Pop (Priority, Bytes, Self.Not_Full, Length, Status);
      end loop;

      case Status is
         when Success =>
            return Success;
         when Too_Small =>
            return Too_Small;
         when Empty =>
            pragma Assert (False, "Should not have exited loop if this was the case.");
            return Error;
      end case;
   exception
      when Program_Error =>
         return Error;
   end Pop_Block;

   function Peek_Length_Block (Self : in out Instance; Priority : out Priority_Type; Length : out Natural) return Peek_Block_Status is
      Status : Peek_Status;
   begin
      Status := Self.Queue.Peek_Length (Priority, Length);
      while Status = Empty loop
         -- Suspend until the queue is no longer empty:
         Ada.Synchronous_Task_Control.Suspend_Until_True (Self.Not_Empty);
         Status := Self.Queue.Peek_Length (Priority, Length);
      end loop;
      return Success;
   exception
      when Program_Error =>
         return Error;
   end Peek_Length_Block;

   -- Byte arrays don't have a "scalar storage order" since they are an array of single byte
   -- items. So this warning doesn't apply. We can safely overlay a byte array with any type
   -- no matter the underlying scalar storage order.
   --
   -- Which is why you will see this below:
   -- pragma Warnings (Off, "overlay changes scalar storage order");

   function Push_Type (Self : in out Instance; Priority : in Priority_Type; Src : in T) return Push_Status is
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Src'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
   begin
      return Self.Push (Priority, Bytes);
   end Push_Type;

   function Push_Type_Block (Self : in out Instance; Priority : in Priority_Type; Src : in T) return Push_Block_Status is
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Src'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
   begin
      return Self.Push_Block (Priority, Bytes);
   end Push_Type_Block;

   function Push_Variable_Length_Type (Self : in out Instance; Priority : in Priority_Type; Src : in T) return Push_Variable_Length_Type_Status is
      use Serializer_Types;
      -- Get the serialized length of the source:
      Num_Bytes_Serialized : Natural;
      Status : constant Serialization_Status := Serialized_Length (Src, Num_Bytes_Serialized);
   begin
      -- Make sure source has a valid length:
      if Status /= Success then
         return Serialization_Failure;
      end if;

      declare
         -- Overlay source type with properly sized byte array:
         subtype Sized_Byte_Array_Index is Natural range 0 .. (Num_Bytes_Serialized - 1);
         subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
         pragma Warnings (Off, "overlay changes scalar storage order");
         Bytes : constant Sized_Byte_Array with Import, Convention => Ada, Address => Src'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
      begin
         case Self.Push (Priority, Bytes) is
            when Success =>
               return Success;
            when Full =>
               return Full;
            when Too_Large =>
               return Too_Large;
         end case;
      end;
   end Push_Variable_Length_Type;

   function Push_Variable_Length_Type_Block (Self : in out Instance; Priority : in Priority_Type; Src : in T) return Push_Variable_Length_Type_Block_Status is
      use Serializer_Types;
      -- Get the serialized length of the source:
      Num_Bytes_Serialized : Natural;
      Status : constant Serialization_Status := Serialized_Length (Src, Num_Bytes_Serialized);
   begin
      -- Make sure source has a valid length:
      if Status /= Success then
         return Serialization_Failure;
      end if;

      declare
         -- Overlay source type with properly sized byte array:
         subtype Sized_Byte_Array_Index is Natural range 0 .. (Num_Bytes_Serialized - 1);
         subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
         pragma Warnings (Off, "overlay changes scalar storage order");
         Bytes : constant Sized_Byte_Array with Import, Convention => Ada, Address => Src'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
      begin
         case Self.Push_Block (Priority, Bytes) is
            when Success =>
               return Success;
            when Too_Large =>
               return Too_Large;
            when Error =>
               return Error;
         end case;
      end;
   end Push_Variable_Length_Type_Block;

   function Pop_Type (Self : in out Instance; Priority : out Priority_Type; Dest : out T) return Pop_Type_Status is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "Priority out parameter initialized by Queue.Pop on success");
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the pop:
      Length : Natural;
      Status : constant Pop_Status := Self.Pop (Priority, Bytes, Length);
   begin
      case Status is
         when Success =>
            -- If the returned length is not what was expected, then return error:
            if Length /= Serialized_Length then
               return Deserialization_Failure;
            else
               return Success;
            end if;
         when Empty =>
            return Empty;
         when Too_Small =>
            return Too_Small;
      end case;
   end Pop_Type;

   function Pop_Type_Block (Self : in out Instance; Priority : out Priority_Type; Dest : out T) return Pop_Type_Block_Status is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "Priority out parameter initialized by Queue.Pop_Block on success");
      -- The length in bytes of the serialized type.
      Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the pop:
      Length : Natural;
      Status : constant Pop_Block_Status := Self.Pop_Block (Priority, Bytes, Length);
   begin
      case Status is
         when Success =>
            -- If the returned length is not what was expected, then return error:
            if Length /= Serialized_Length then
               return Deserialization_Failure;
            else
               return Success;
            end if;
         when Too_Small =>
            return Too_Small;
         when Error =>
            return Error;
      end case;
   end Pop_Type_Block;

   function Pop_Variable_Length_Type (Self : in out Instance; Priority : out Priority_Type; Dest : out T) return Pop_Type_Status is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "Priority out parameter initialized by Queue.Pop on success");
      -- The length in bytes of the serialized type.
      Max_Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Max_Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the pop:
      Length : Natural;
      Status : constant Pop_Status := Self.Pop (Priority, Bytes, Length);
   begin
      case Status is
         when Success =>
            declare
               use Serializer_Types;
               -- Get the serialized length of the destination:
               Num_Bytes_Serialized : Natural;
               Ser_Status : constant Serialization_Status := Serialized_Length (Dest, Num_Bytes_Serialized);
            begin
               -- If getting the serialized length failed or if the serialized length returned is larger then
               -- the number of bytes returned from the pop then return error:
               if Ser_Status /= Success or else Num_Bytes_Serialized > Length then
                  return Deserialization_Failure;
               else
                  return Success;
               end if;
            end;
         when Empty =>
            return Empty;
         when Too_Small =>
            return Too_Small;
      end case;
   end Pop_Variable_Length_Type;

   function Pop_Variable_Length_Type_Block (Self : in out Instance; Priority : out Priority_Type; Dest : out T) return Pop_Type_Block_Status is
      pragma Annotate (GNATSAS, False_Positive, "validity check", "Priority out parameter initialized by Queue.Pop_Block on success");
      -- The length in bytes of the serialized type.
      Max_Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
      -- Byte_Array type for storing the type:
      subtype Byte_Array_Index is Natural range 0 .. (Max_Serialized_Length - 1);
      subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
      -- Optimization: create a byte array that overlays the data variable then
      -- pass this byte array into the push function. This avoids a double copy of the data:
      pragma Warnings (Off, "overlay changes scalar storage order");
      Bytes : Byte_Array with Import, Convention => Ada, Address => Dest'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- Do the pop:
      Length : Natural;
      Status : constant Pop_Block_Status := Self.Pop_Block (Priority, Bytes, Length);
   begin
      case Status is
         when Success =>
            declare
               use Serializer_Types;
               -- Get the serialized length of the destination:
               Num_Bytes_Serialized : Natural;
               Ser_Status : constant Serialization_Status := Serialized_Length (Dest, Num_Bytes_Serialized);
            begin
               -- If getting the serialized length failed or if the serialized length returned is larger then
               -- the number of bytes returned from the pop then return error:
               if Ser_Status /= Success or else Num_Bytes_Serialized > Length then
                  return Deserialization_Failure;
               else
                  return Success;
               end if;
            end;
         when Too_Small =>
            return Too_Small;
         when Error =>
            return Error;
      end case;
   end Pop_Variable_Length_Type_Block;

   function Num_Bytes_Free (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Num_Bytes_Free;
   end Num_Bytes_Free;

   function Num_Bytes_Used (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Num_Bytes_Used;
   end Num_Bytes_Used;

   function Max_Num_Bytes_Used (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Max_Num_Bytes_Used;
   end Max_Num_Bytes_Used;

   function Size_In_Bytes (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Num_Bytes_Total;
   end Size_In_Bytes;

   function Current_Percent_Used (Self : in out Instance) return Basic_Types.Byte is
   begin
      return Self.Queue.Current_Percent_Used;
   end Current_Percent_Used;

   function Max_Percent_Used (Self : in out Instance) return Basic_Types.Byte is
   begin
      return Self.Queue.Max_Percent_Used;
   end Max_Percent_Used;

   function Num_Elements (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Get_Count;
   end Num_Elements;

   function Max_Num_Elements (Self : in out Instance) return Natural is
   begin
      return Self.Queue.Get_Max_Count;
   end Max_Num_Elements;

end Protected_Priority_Queue;
