# Seq opcode overview
### This is a reference for SEQ opcodes and their bit patterns, which may or may not be useful. Preview ctrl + shift + V.
### This is all defined in the SEQ repository at ext/include/seq_instr.h

## Command Opcodes and Bit Patterns
| Opcode | Command Name           | Bit Pattern                         | Bit Pattern Description                                | Implementation Status |
| ------ |:-----------------------|:------------------------------------|--------------------------------------------------------|-----------------------|
| 0      | Set Bit Pattern        | U8 - U8 - U16                       | Opcode - Pad - Length                                  |         Done          |
| 1      | Send Bit Pattern       | U8 - U8 - U8 - U8                   | Opcode - Pad - Pad - Pad                               |         Done          | 
| 2      | Update Bit Pattern     | U8 - U8 - U16 - U32                 | Opcode - Pad - Offset - Length                         |         Done          |
| 3      | Call                   | U8 - U8 - U8 - U8 - char[20]        | Opcode - Pad - Pad - Pad - Name                        |         Done          | 
| 4      | Spawn                  | U8 - U8 - U8 - U8 - char[20]        | Opcode - Pad - Pad - Pad - Name                        |         Done          |
| 5      | Start                  | U8 - U8 - U8 - U8 - char[20]        | Opcode - Pad - Pad - Pad - Name                        |         Done          |
| 6      | Push                   | U8 - U8 - U8 - U8                   | Opcode - Engine - destination ID - Pad                 |         Done          |
| 7      | Eval                   | U8 - U8 - U8 - U8                   | Opcode - Op - Pad - Pad                                |         Done          |
| 8      | Fetch Var A            | U8 - U8 - U8 - U8 - Struct          | Opcode - Pad - Pad - Pad - varInfo_T                   |         Done          |
| 9      | Fetch Var B            | U8 - U8 - U8 - U8 - Struct          | Opcode - Pad - Pad - Pad - varInfo_T                   |         Done          |
| 10     | Store Var              | U8 - U8 - U8 - U8 - Struct          | Opcode - Pad - Pad - Pad - varInfo_T                   |         Done          |
| 11     | Fetch TLM A            | U8 - U8 - U8 - U8 - Struct          | Opcode - Pad - Pad - Pad - tlmInfo_T                   |         Done          |
| 12     | Fetch TLM B            | U8 - U8 - U8 - U8 - Struct          | Opcode - Pad - Pad - Pad - tlmInfo_T                   |         Done          |
| 13     | No Command (Invalid)   | ----------------------------------- | ------------------------------------------------------ |         N/A           |
| 14     | Wait                   | U8 - U8 - U8 - U8 - U32             | Opcode - waitType - Pad - Pad - waitTime               |         Done          |
| 15     | Goto                   | U8 - U8 - U16                       | Opcode - Pad - Position                                |         Done          |
| 16     | Jump If Zero           | U8 - U8 - U16                       | Opcode - Pad - Position                                |         Done          |
| 17     | Jump Not Zero          | U8 - U8 - U16                       | Opcode - Pad - Position                                |         Done          |
| 18     | Jump If Equal          | U8 - U8 - U16 - U32                 | Opcode - Pad - Position - Value                        |         Done          |
| 19     | Jump Not Equal         | U8 - U8 - U16 - U32                 | Opcode - Pad - Position - Value                        |         Done          |
| 20     | Return                 | U8 - U8 - U8 - U8                   | Opcode - Pad - Pad - Pad                               |         Done          |
| 21     | Wait If Zero           | U8 - U8 - U16 - U32                 | Opcode - waitType - Position - Timeout                 |         Done          |
| 22     | Kill Cat               | U8 - U8 - U16                       | Opcode - Pad - Category                                |     Unimplemented     |
| 23     | Kill Eng               | U8 - U8 - U8 - U8                   | Opcode - EngStart - EngNum - Pad                       |         Done          |
| 24     | Kill Name              | U8 - U8 - U8 - U8 - char[20]        | Opcode - Pad - Pad - Pad - Name                        |     Unimplemented     |
| 25     | Subscribe              | U8 - U8 - U16                       | Opcode - Pad - Message ID                              |     Unimplemented     |
| 26     | Unsubscribe            | U8 - U8 - U16                       | Opcode - Pad - Message ID                              |     Unimplemented     |
| 27     | Eval FLT               | U8 - U8 - U8 - U8                   | Opcode - Op - Pad - Pad                                |         Done          |
| 28     | Cast F to U            | U8 - U8 - U8 - U8                   | Opcode - ID - Pad - Pad                                |         Done          |
| 29     | Cast U to F            | U8 - U8 - U8 - U8                   | Opcode - ID - Pad - Pad                                |         Done          |
| 30     | Eval S                 | U8 - U8 - U8 - U8                   | Opcode - Op - Pad - Pad                                |         Done          |
| 31     | Cast S to U            | U8 - U8 - U8 - U8                   | Opcode - ID - Pad - Pad                                |         Done          |
| 32     | Cast U to S            | U8 - U8 - U8 - U8                   | Opcode - ID - Pad - Pad                                |         Done          |
| 33     | Cast F to S            | U8 - U8 - U8 - U8                   | Opcode - ID - Pad - Pad                                |         Done          |
| 34     | Cast S to F            | U8 - U8 - U8 - U8                   | Opcode - ID - Pad - Pad                                |         Done          |
| 35     | Wait on B              | U8 - U8 - U8 - U8                   | Opcode - waitType - Pad - Pad                          |         Done          |
| 36     | Wait If Zero On B      | U8 - U8 - U16                       | Opcode - waitType - Position                           |         Done          |
| 37     | Str Alloc              | U8 - U8 - U8 - U8                   | Opcode - ID - Pad - Pad                                |     Unimplemented     |
| 38     | Str Dealloc            | U8 - U8 - U8 - U8 - Struct          | Opcode - Pad - Pad - Pad - varInfo_T                   |     Unimplemented     |
| 39     | Str Set                | U8 - U8 - U8 - U8 - Struct - U8[64] | Opcode - Pad - Pad - Pad - varInfo_T - str             |     Unimplemented     |
| 40     | Str Update Bit Pattern | U8 - U8 - U16 - U32                 | Opcode - Pad - Offset - Length                         |     Unimplemented     |
| 41     | Str Copy               | U8 - U8 - U8 - U8 - Struct - Struct | Opcode - Pad - Pad - Pad - varInfo_T(s) - varInfo_T(d) |     Unimplemented     |
| 42     | Str Move               | U8 - U8 - U8 - U8 - Struct          | Opcode - Pad - Pad - Pad - varInfo_T                   |     Unimplemented     |
| 43     | Print                  | U8 - U8 - U8 - U8 - char[60]        | Opcode - Type - Pad - Pad - str                        |         Done          |
| 44     | Print Var              | U8 - U8 - U8 - U8 - Struct          | Opcode - Type - Pad - Pad - varInfo_T                  |         Done          |
| 45     | Print Str              | U8 - U8 - U8 - U8 - Struct          | Opcode - Type - Pad - Pad - varInfo_T                  |     Unimplemented     |

## Command Opcodes Descriptions
| Opcode | Command Name           | Description                                                                                                          |
| ------ |:-----------------------|:---------------------------------------------------------------------------------------------------------------------|
| 0      | Set Bit Pattern        | Extract command from sequence and store it in internal command buffer.                                               | 
| 1      | Send Bit Pattern       | Send command stored in internal command buffer.                                                                      |
| 2      | Update Bit Pattern     | Update the internal command buffer (command arguments) with the value from a SEQ variable.                           |
| 3      | Call                   | Load and run a subsequence in this engine.                                                                           |
| 4      | Spawn                  | Load and run a sequence in another engine.                                                                           |
| 5      | Start                  | Load and run a sequence in this engine, replacing the currently running sequence.                                   |
| 6      | Push                   | Copy a SEQ variable to the argument buffer to prepare to call/spawn/start a new sequence.                            |
| 7      | Eval                   | Perform an unsigned integer operation on internal variables A and B.                                                 |
| 8      | Fetch Var A            | Fetch a SEQ variable or constant and store it into internal variable A.                                              |
| 9      | Fetch Var B            | Fetch a SEQ variable or constant and store it into internal variable B.                                              |
| 10     | Store Var              | Store internal A into a SEQ variable.                                                                                |
| 11     | Fetch TLM A            | Fetch a telemetry value and store it into internal variable A.                                                       |
| 12     | Fetch TLM B            | Fetch a telemetry value and store it into internal variable B.                                                       |
| 13     | No Command (Invalid)   | 13 is not a valid op code.                                                                                           |
| 14     | Wait                   | Wait for an absolute time or a relative amount of time from the current time.                                        |
| 15     | Goto                   | Jump to a specific instruction and begin executing there.                                                            |
| 16     | Jump If Zero           | Jump to an instruction if internal variable A is equal to zero.                                                      |
| 17     | Jump Not Zero          | Jump to an instruction if internal variable A is not equal to zero.                                                  |
| 18     | Jump If Equal          | Jump to an instruction if internal variable A is equal to a specific value.                                          |
| 19     | Jump Not Equal         | Jump to an instruction if internal variable A is not equal to a specific value.                                      |
| 20     | Return                 | Copy internal variable A to the special internal return variable and finish executing the sequence.                  |
| 21     | Wait If Zero           | Continue waiting if internal variable A is equal to zero, check timeout.                                             |
| 22     | Kill Cat               | Not implemented, sequence categories not supported.                                                                  |
| 23     | Kill Eng               | Kill the sequence (and any subsequences) running in a sequence engine.                                               |
| 24     | Kill Name              | Not implemented.                                                                                                     |
| 25     | Subscribe              | Not applicable to Adamant systems.                                                                                   |
| 26     | Unsubscribe            | Not applicable to Adamant systems.                                                                                   |
| 27     | Eval FLT               | Perform a floating point operation on internal variables A and B.                                                    |
| 28     | Cast F to U            | Convert a floating point value to an unsigned integer.                                                               |
| 29     | Cast U to F            | Convert an unsigned integer to a floating point value.                                                               |
| 30     | Eval S                 | Perform a signed integer operation on internal variables A and B.                                                    |
| 31     | Cast S to U            | Convert a signed integer to an unsigned integer.                                                                     |
| 32     | Cast U to S            | Convert an unsigned integer to a signed integer.                                                                     |
| 33     | Cast F to S            | Convert a floating point value to a signed integer.                                                                  |
| 34     | Cast S to F            | Convert a signed integer to a floating point value.                                                                  |
| 35     | Wait on B              | Wait for an absolute time or relative amount of time from the current time, stored in variable B.                    |
| 36     | Wait If Zero On B      | Continue waiting if internal variable A is equal to zero, check timeout which is stored in internal variable B.      |
| 37     | Str Alloc              | Not implemented, string pool not supported.                                                                          |
| 38     | Str Dealloc            | Not implemented, string pool not supported.                                                                          |
| 39     | Str Set                | Not implemented, string pool not supported.                                                                          |
| 40     | Str Update Bit Pattern | Not implemented, string pool not supported.                                                                          |
| 41     | Str Copy               | Not implemented, string pool not supported.                                                                          |
| 42     | Str Move               | Not implemented, string pool not supported.                                                                          |
| 43     | Print                  | Print a string, implemented as a FSW event message.                                                                  |
| 44     | Print Var              | Print a SEQ variable, implemented as a FSW event message.                                                            |
| 45     | Print Str              | Not implemented, string pool not supported.                                                                          |

## Other Seq information
| Name | Bit Pattern | Bit Pattern Description |
|--------|:----------------|------------------------|
| Header | U32 - U16 - U16 | versionInfo - category - length |
| varInfo | U32 - U16 - U16 | ID - Type - Pad |
| tlmInfo | U16 - U16 - U16 - U16 | apid - offset - length - pad |

