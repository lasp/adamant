with Ada.Unchecked_Conversion;
with Basic_Types;
with Interfaces; use Interfaces;
with System.Storage_Elements;

package body Double_Buffer_Store with SPARK_Mode => On is

   -- The same policy as the spec, so the contracts here are proof only too:
   pragma Assertion_Policy
      (Pre => Ignore,
       Pre'Class => Ignore,
       Post => Ignore,
       Post'Class => Ignore,
       Ghost => Ignore);

   use type Basic_Types.Byte_Array;

   -- A plain copy of what a region holds. Reading a region into one of these touches
   -- the volatile memory once, and everything else works on the copy.
   type Copy_Contents is record
      Crc : Crc_16.Crc_16_Type;
      Payload : Payload_Type;
   end record;

   -- The bytes of a T, for the CRC and for the zero value. The unchecked conversions
   -- are legal in SPARK because every bit pattern of T is valid, which GNATprove checks
   -- at each instantiation. Universal_Aliasing lets the compiler convert without a
   -- copy. GNATprove does not model it and says so, which is harmless, so that message
   -- is silenced.
   pragma Warnings (GNATprove, Off, "*Universal_Aliasing*");
   type Data_Bytes is new Basic_Types.Byte_Array (0 .. T'Object_Size / Basic_Types.Byte'Object_Size - 1)
      with Universal_Aliasing;
   pragma Warnings (GNATprove, On, "*Universal_Aliasing*");
   function To_Bytes is new Ada.Unchecked_Conversion (Source => T, Target => Data_Bytes);

   -- The CRC covers the serialized save time followed by the bytes of the data:
   function Compute_Crc (Payload : in Payload_Type) return Crc_16.Crc_16_Type is
      (Crc_16.Compute_Crc_16 (Sys_Time.Serialization.To_Byte_Array (Payload.Save_Time) & Basic_Types.Byte_Array (To_Bytes (Payload.Data))))
      with Global => null;

   -------------------------------------------------------------------------
   -- Memory access. These are the only subprograms that touch the regions.
   -------------------------------------------------------------------------

   -- Bind the store to its regions and check that they do not overlap. This is the
   -- one operation outside SPARK, which forbids copying an access value to volatile
   -- memory and taking addresses. Its postcondition is what the rest of the store is
   -- proved against.
   procedure Bind (Self : out Instance; Region_A : in not null Persistent_Copy_Access; Region_B : in not null Persistent_Copy_Access)
      with Global => null,
           Post => Is_Initialized (Self);

   procedure Bind (Self : out Instance; Region_A : in not null Persistent_Copy_Access; Region_B : in not null Persistent_Copy_Access)
      with SPARK_Mode => Off
   is
      use System.Storage_Elements;
      Size : constant Integer_Address := Integer_Address (Persistent_Copy'Max_Size_In_Storage_Elements);
      Start_A : constant Integer_Address := To_Integer (Region_A.all'Address);
      Start_B : constant Integer_Address := To_Integer (Region_B.all'Address);
   begin
      pragma Assert (Start_A + Size <= Start_B or else Start_B + Size <= Start_A, "The two copies of a store must not overlap.");
      Self := (Region_A => Region_A, Region_B => Region_B, Target => Copy_A);
   end Bind;

   procedure Read_Region (Region : in Persistent_Copy; Contents : out Copy_Contents)
      with Global => null
   is
   begin
      Contents := (Crc => Region.Crc, Payload => Region.Payload);
   end Read_Region;

   -- Payload first and CRC last, as two statements, so the CRC reaches memory only
   -- after the data it covers:
   procedure Write_Region (Region : out Persistent_Copy; Contents : in Copy_Contents)
      with Global => null
   is
   begin
      Region.Payload := Contents.Payload;
      Region.Crc := Contents.Crc;
   end Write_Region;

   -- Complement the CRC so the copy cannot read as valid until it is written again:
   procedure Invalidate_Region (Region : in out Persistent_Copy)
      with Global => null
   is
      Crc : constant Crc_16.Crc_16_Type := Region.Crc;
   begin
      Region.Crc := [not Crc (0), not Crc (1)];
   end Invalidate_Region;

   -- The same three operations on one of the store's copies:
   procedure Read (Self : in Instance; Copy : in Copy_Type; Contents : out Copy_Contents)
      with Global => null,
           Pre => Is_Initialized (Self)
   is
   begin
      case Copy is
         when Copy_A => Read_Region (Region => Self.Region_A.all, Contents => Contents);
         when Copy_B => Read_Region (Region => Self.Region_B.all, Contents => Contents);
      end case;
   end Read;

   procedure Write (Self : in out Instance; Copy : in Copy_Type; Contents : in Copy_Contents)
      with Global => null,
           Pre => Is_Initialized (Self),
           Post => Is_Initialized (Self)
   is
   begin
      case Copy is
         when Copy_A => Write_Region (Region => Self.Region_A.all, Contents => Contents);
         when Copy_B => Write_Region (Region => Self.Region_B.all, Contents => Contents);
      end case;
   end Write;

   procedure Invalidate (Self : in out Instance; Copy : in Copy_Type)
      with Global => null,
           Pre => Is_Initialized (Self),
           Post => Is_Initialized (Self)
   is
   begin
      case Copy is
         when Copy_A => Invalidate_Region (Region => Self.Region_A.all);
         when Copy_B => Invalidate_Region (Region => Self.Region_B.all);
      end case;
   end Invalidate;

   -------------------------------------------------------------------------
   -- Copy selection:
   -------------------------------------------------------------------------

   -- Read both copies and return the data of the newest valid one. Source names that
   -- copy, or Neither if no copy is valid, in which case Value is all zero bits.
   procedure Try_Restore (Self : in Instance; Value : out T; Source : out Source_Type)
      with Global => null,
           Pre => Is_Initialized (Self)
   is
      function From_Bytes is new Ada.Unchecked_Conversion (Source => Data_Bytes, Target => T);

      function Is_Valid (Contents : in Copy_Contents) return Boolean is
         (Contents.Crc = Compute_Crc (Contents.Payload));

      function Is_Later (Time : in Sys_Time.T; Than : in Sys_Time.T) return Boolean is
         (Time.Seconds > Than.Seconds or else (Time.Seconds = Than.Seconds and then Time.Subseconds > Than.Subseconds));

      Current_A : Copy_Contents;
      Current_B : Copy_Contents;
      A_Valid : Boolean;
      B_Valid : Boolean;
   begin
      Read (Self => Self, Copy => Copy_A, Contents => Current_A);
      Read (Self => Self, Copy => Copy_B, Contents => Current_B);
      A_Valid := Is_Valid (Current_A);
      B_Valid := Is_Valid (Current_B);

      -- Copy A wins a tie:
      if A_Valid and then B_Valid then
         Source := (if Is_Later (Current_B.Payload.Save_Time, Than => Current_A.Payload.Save_Time) then Copy_B else Copy_A);
      elsif A_Valid then
         Source := Copy_A;
      elsif B_Valid then
         Source := Copy_B;
      else
         Source := Neither;
      end if;

      case Source is
         when Copy_A =>
            Value := Current_A.Payload.Data;
         when Copy_B =>
            Value := Current_B.Payload.Data;
         when Neither =>
            Value := From_Bytes (Data_Bytes'[others => 0]);
      end case;
   end Try_Restore;

   -------------------------------------------------------------------------
   -- Public operations:
   -------------------------------------------------------------------------

   function Other_Copy (Copy : in Copy_Type) return Copy_Type is
      (case Copy is
         when Copy_A => Copy_B,
         when Copy_B => Copy_A);

   procedure Init (
      Self : out Instance;
      Region_A : in not null Persistent_Copy_Access;
      Region_B : in not null Persistent_Copy_Access;
      Value : out T;
      Status : out Restore_Status
   ) is
      Source : Source_Type;
   begin
      Bind (Self => Self, Region_A => Region_A, Region_B => Region_B);
      Try_Restore (Self => Self, Value => Value, Source => Source);
      case Source is
         when Copy_A | Copy_B =>
            Status := Restored;
            -- The other copy is stale. Invalidate it and write it next.
            Invalidate (Self => Self, Copy => Other_Copy (Source));
            Self.Target := Other_Copy (Source);
         when Neither =>
            Status := No_Valid_Copy;
            Self.Target := Copy_A;
      end case;
   end Init;

   procedure Restore (Self : in Instance; Value : out T; Status : out Restore_Status) is
      Source : Source_Type;
   begin
      Try_Restore (Self => Self, Value => Value, Source => Source);
      case Source is
         when Copy_A | Copy_B =>
            Status := Restored;
         when Neither =>
            Status := No_Valid_Copy;
      end case;
   end Restore;

   procedure Save (Self : in out Instance; Value : in T; Save_Time : in Sys_Time.T) is
      Payload : constant Payload_Type := (Save_Time => Save_Time, Data => Value);
      Target : constant Copy_Type := Self.Target;
   begin
      Write (Self => Self, Copy => Target, Contents => (Crc => Compute_Crc (Payload), Payload => Payload));
      Self.Target := Other_Copy (Target);
   end Save;

end Double_Buffer_Store;
