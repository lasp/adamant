--------------------------------------------------------------------------------
-- Product_Database Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Data_Product;
with Data_Product_Return;
with Data_Product_Fetch;
with Command;
with Variable_Database;
with Basic_Enums;

-- The product database component maintains a database of data product items. Only the latest single copy of each data product item is stored, and that value can be updated or fetched by ID via connectors. The component is configured by passing the minimum and maximum data product ID that the database can accept. The component allocates memory on the heap to store a maximum sized data product for every ID in range from the minimum to maximum ID provided. Invalid IDs received during requests are reported as events. The lookup algorithm is extremely fast, using the data product ID itself as a direct index into the database.
--
-- Note that IDs stored in this database should come from a compact ID space for most efficient memory usage. If you are manually setting the data product ID bases in your assembly model and creating a sparse ID set than this database component should not be used, as it could waste an enormous amount of memory. This component is designed to work best with the default, Adamant-allocated ID space for data products which spans from 1 to number of data products used in the system.
package Component.Product_Database.Implementation is

   -- The component class instance record:
   type Instance is new Product_Database.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires the minimum and maximum acceptable data product IDs in order to size its internal database. Memory will be allocated to store a maximum sized data product for every ID in the range provided.
   --
   -- Init Parameters:
   -- Minimum_Data_Product_Id : Data_Product_Types.Data_Product_Id - The minimum data product identifier that the database will accept.
   -- Maximum_Data_Product_Id : Data_Product_Types.Data_Product_Id - The maximum data product identifier that the database will accept. This value combined with the Minimum_Data_Product_Id are used to allocate a table on the heap. Ids stored in this database should come from a compact Id space for most efficient memory usage.
   -- Send_Event_On_Missing : Boolean - By default the product database will send an event every time a data product is fetched that is missing. Sometimes this is expected behavior and the message is annoying. This flag allows that event to be disabled permanently on startup if needed.
   --
   overriding procedure Init (Self : in out Instance; Minimum_Data_Product_Id : in Data_Product_Types.Data_Product_Id; Maximum_Data_Product_Id : in Data_Product_Types.Data_Product_Id; Send_Event_On_Missing : in Boolean := True);
   not overriding procedure Final (Self : in out Instance);

private

   -- Instantiation of generic database package to a package specifically for holding
   -- data products accessed by data product id:
   package Data_Product_Database is new Variable_Database (Data_Product_Types.Data_Product_Id, Data_Product.T, Data_Product.Serialized_Length, Data_Product.Serialized_Length);

   -- Protected version of the database:
   -- This protected type is just a wrapper around the unprotected database data structure.
   -- For information on these functions, see the database source.
   protected type Protected_Database is
      -- Procedures requiring full mutual exclusion:
      procedure Init (Minimum_Id : in Data_Product_Types.Data_Product_Id; Maximum_Id : in Data_Product_Types.Data_Product_Id);
      procedure Destroy;
      procedure Update (Id : in Data_Product_Types.Data_Product_Id; Value : in Data_Product.T; Status : out Data_Product_Database.Update_Status);
      function Fetch (Id : in Data_Product_Types.Data_Product_Id; Value : out Data_Product.T) return Data_Product_Database.Fetch_Status;
      procedure Override (Id : in Data_Product_Types.Data_Product_Id; Value : in Data_Product.T; Status : out Data_Product_Database.Update_Status);
      procedure Clear_Override (Id : in Data_Product_Types.Data_Product_Id; Status : out Data_Product_Database.Clear_Override_Status);
      procedure Clear_Override_All;
      function Overridden return Basic_Enums.Enable_Disable_Type.E;
   private
      -- Protected database data structure:
      Db : Data_Product_Database.Instance;
   end Protected_Database;

   -- The component class instance record:
   type Instance is new Product_Database.Base_Instance with record
      Db : Protected_Database;
      Send_Event_On_Missing : Boolean := True;
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
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Store a data product item in the database.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Fetch a data product item from the database.
   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T;
   -- This is the command receive connector. This does not need to be connected if the command for this component will not be used.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Product Database.
   -- Clear the override condition for the data product of the provided ID.
   overriding function Clear_Override (Self : in out Instance; Arg : in Data_Product_Id.T) return Command_Execution_Status.E;
   -- Clear the override condition for all data products in the product store.
   overriding function Clear_Override_For_All (Self : in out Instance) return Command_Execution_Status.E;
   -- Override the value of a data product in the data product store. The value of this data product will be fixed to the commanded value, ignoring all other updates, until the override is cleared.
   overriding function Override (Self : in out Instance; Arg : in Data_Product.T) return Command_Execution_Status.E;
   -- Dump the data product of the provided ID in a packet.
   overriding function Dump (Self : in out Instance; Arg : in Data_Product_Id.T) return Command_Execution_Status.E;
   -- Dump the data product of the provided ID into a poly type based on the provided offset and length.
   overriding function Dump_Poly_Type (Self : in out Instance; Arg : in Data_Product_Poly_Extract.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Product_Database.Implementation;
