# ODMB7/5 Development

This repository contains the ODMB7/5 firmware in development.

## Structure

### ODMB7/5 firmware
Most (if not all) of the sub-modules are shared between ODMB7 and ODMB5, with generic variable NCFEB to define any difference in behaviors between them.
The top modules of ODMB7/5 firmwares are contained in [odmb7_dev_top.vhd](/source/odmb7_dev_top.vhd) and [odmb5_dev_top.vhd](/source/odmb5_dev_top.vhd), respectively.
Their project may also contain different set of files and IPs, managed by different Vivado projects, [odmb7_ucsb_dev.xpr](/project/odmb7_ucsb_dev.xpr) and [odmb5_ucsb_dev.xpr](/project/odmb5_ucsb_dev.xpr).

#### I/O signals naming convention
- The naming of the signals in the top modules follow the signal names that connected directly to the FPGA in the respective schematics as much as possible, except when they can be
  improved to give more clarity. In such case (e.g. `C_TDO` --> `DCFEB_TDO`, `DONE` --> `DCFEB_DONE`), the actual signal name is listed in the comment of the top file.

- The position of the pin (which Bank it is connected to) shall be attached as comment in the entity declaration.

- Every connected signal is assigned a corresponding signal in the top module entity declaration to keep the record, even if they are unused. An exception is made to the `EMCCLK` 
  pin and the primary PROM programming pins.

### Simulation testbench
The HDL code specific for simulation are under the [simulation](/simulation) folder. This will contain the simulation wrapper for ODMB7/5 firmware, the simulated VME, DCFEBs, and possibly
other devices in the future. LUTs are used to provide VME commands to the ODMB7/5.

### Other resources
#### Clock synthesizer config
The most recent config file for the clock synthesizer on board, as well as the human readable documentation of them are placed under the [clock_configs](/resources/clock_configs) directory.

Currently, the firmware is developed under the assumption that config similar to `ZL30267_4freqs_211115.mfg` is used.

## Progress tracking

- [ ] Port ODMB_VME from ODMB
  - [X] COMMAND_MODULE
  - [X] Device 1: CFEBJTAG
    - [X] Import VME simulation
    - [X] Import DCFEB simulation (only user code reading)
  - [X] Device 2: ODMBJTAG
  - [X] Device 3: VMEMON
  - [X] Device 4: VMECONFREGS
  - [ ] Device 5: TESTFIFOS --> CLOCK_MON
  - [X] Device 6: SPI_PORT
    - [X] CFG Register upload/download
    - [X] Write Command FIFO
    - [X] Read readback FIFO
    - [X] SPI state machine commands
    - [X] Read SPI status/timer
  - [X] Device 7: SYSTEM_MON
    - [X] Import SYSMON module for currents
    - [X] Develope voltage monitoring with SPI 
  - [X] Device 8: LVDBMON
    - [X] Import LVDB module
  - [ ] Device 9: SYSTEM_TEST
    - [X] Import OTMB PRBS test
    - [ ] Import the Optical PRBS tests
  - [X] SPI_CTRL
    - [X] Read/write/erase PROM commands
    - [X] Other PROM commands (status/lock/unlock/check)
    - [X] Timer commands

- [ ] Port ODMB_CTRL from ODMB
  - [ ] Run3 logic tested with Cosmics

- [ ] Configure optical interfaces
  - [X] (x)DCFEB interface
  - [ ] ALCT interface
    - [ ] GBT interface between ALCT-LX100 and ODMB7
    - [ ] Dual link 8B/10B between ALCT-LX150 and ODMB5
  - [ ] SPY interface
    - [X] DDU communication logic for Run3 DAQ config
    - [X] PC communication logic -- usage not well defined yet
  - [ ] FED interface
    - [ ] Quad-link data transmission to the FED for ODMB7
    - [ ] Tri-link and dual-link data transmission for ODMB5
    - [ ] GBT interface for back pressure signal from FED
  
- [ ] Wrap up dangling logics in the top file
  - [X] ODMB clocking logic 
  - [X] ODMB status monitoring logic 
  - [ ] ODMB reset and init logic

- [ ] Develop Run4 DAQ logic

