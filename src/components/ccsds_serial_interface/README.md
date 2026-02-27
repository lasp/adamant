## CCSDS Serial Interface

#### Description

This directory currently contains one serial interface component. This component is meant to be a "backdoor" serial
component which uses Ada.Text_IO to send and receive data over a serial port. On Linux, this will send/recv data 
to/from the terminal, but Ada.Text_IO is attached to a diagnostic uart on most embedded systems. This means that
this component can be used as a quick and dirty serial interface without implementing hardware specific uart drivers.

