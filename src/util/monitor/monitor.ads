
-- A simple monitor package which consists of two states, green and red, and
-- two persistances which determine the transition between them. A true
-- predicate encourages the monitor to be in the green state, a false predicate
-- causes an eventual transition to the red state.
package Monitor is

   type Instance is tagged private;

   type Monitor_State is (Green, Red, Disabled);
   type Check_Status is (Green, Green_To_Red, Red, Red_To_Green, Disabled);

   -- Initialization:
   procedure Init (Self : in out Instance; Green_To_Red_Persistance_Threshold : in Positive; Red_To_Green_Persistance_Threshold : in Positive; Enabled : in Boolean := True);
   procedure Set_Persistance_Thresholds (Self : in out Instance; Green_To_Red_Persistance_Threshold : in Positive; Red_To_Green_Persistance_Threshold : in Positive);

   -- Enable/disable functions:
   procedure Enable (Self : in out Instance);
   procedure Disable (Self : in out Instance);

   -- Monitoring functions:
   function Check (Self : in out Instance; Predicate : in Boolean) return Check_Status;
   function Get_State (Self : in Instance) return Monitor_State;

private

   type Instance is tagged record
      State : Monitor_State := Green;
      Persistence_Count : Natural := Natural'First;
      Green_To_Red_Threshold : Positive := Positive'First;
      Red_To_Green_Threshold : Positive := Positive'First;
   end record;

end Monitor;
