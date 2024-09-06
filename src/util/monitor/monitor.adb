package body Monitor is

   procedure Init (Self : in out Instance; Green_To_Red_Persistance_Threshold : in Positive; Red_To_Green_Persistance_Threshold : in Positive; Enabled : in Boolean := True) is
   begin
      -- Set internal state:
      if Enabled then
         Self.State := Green;
      else
         Self.State := Disabled;
      end if;

      -- Set the persistence thresholds:
      Self.Set_Persistance_Thresholds (
         Green_To_Red_Persistance_Threshold => Green_To_Red_Persistance_Threshold,
         Red_To_Green_Persistance_Threshold => Red_To_Green_Persistance_Threshold
      );
   end Init;

   procedure Set_Persistance_Thresholds (Self : in out Instance; Green_To_Red_Persistance_Threshold : in Positive; Red_To_Green_Persistance_Threshold : in Positive) is
   begin
      -- Set the thresholds:
      Self.Green_To_Red_Threshold := Green_To_Red_Persistance_Threshold;
      Self.Red_To_Green_Threshold := Red_To_Green_Persistance_Threshold;

      -- Reset the internal counter:
      Self.Persistence_Count := Natural'First;

      -- If the state is set to red, then reset it to green. If it is set
      -- to disabled, then leave it.
      case Self.State is
         when Red => Self.State := Green;
         when Green => null;
         when Disabled => null;
      end case;
   end Set_Persistance_Thresholds;

   procedure Enable (Self : in out Instance) is
   begin
      Self.State := Green;

      -- Reset the internal counter:
      Self.Persistence_Count := Natural'First;
   end Enable;

   procedure Disable (Self : in out Instance) is
   begin
      Self.State := Disabled;

      -- Reset the internal counterss:
      Self.Persistence_Count := Natural'First;
   end Disable;

   function Check (Self : in out Instance; Predicate : in Boolean) return Check_Status is
   begin
      -- Return immediately if the monitor is disabled:
      case Self.State is
         when Disabled =>
            -- Return immediately if the monitor is disabled:
            return Disabled;

         when Green =>
            -- See if we need to start transitioning to red:
            if Predicate then
               -- Reset counter:
               Self.Persistence_Count := Natural'First;
               return Green;
            else
               -- Increment counter:
               if Self.Persistence_Count < Natural'Last then
                  Self.Persistence_Count := @ + 1;
               end if;

               -- Transition if counter is at the threshold:
               if Self.Persistence_Count >= Self.Green_To_Red_Threshold then
                  Self.State := Red;
                  Self.Persistence_Count := Natural'First;
                  return Green_To_Red;
               else
                  return Green;
               end if;
            end if;

         when Red =>
            -- See if we need to start transitioining to green:
            if Predicate then
               -- Increment counter:
               if Self.Persistence_Count < Natural'Last then
                  Self.Persistence_Count := @ + 1;
               end if;

               -- Transition if counter is at the threshold:
               if Self.Persistence_Count >= Self.Red_To_Green_Threshold then
                  Self.State := Green;
                  Self.Persistence_Count := Natural'First;
                  return Red_To_Green;
               else
                  return Red;
               end if;
            else
               -- Reset counter:
               Self.Persistence_Count := Natural'First;
               return Red;
            end if;
      end case;
   end Check;

   function Get_State (Self : in Instance) return Monitor_State is
   begin
      return Self.State;
   end Get_State;

end Monitor;
