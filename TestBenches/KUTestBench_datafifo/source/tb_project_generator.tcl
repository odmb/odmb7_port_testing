# In the source directory run the below command
# vivado -nojournal -nolog -mode batch -source tb_project_generator.tcl

# Environment variables
set FPGA_TYPE xcku040-ffva1156-2-e

# Generate ip
set argv $FPGA_TYPE
set argc 1
source ip_generator.tcl

# Create project
create_project tb_project ../tb_project -part $FPGA_TYPE -force
set_property target_language VHDL [current_project]
set_property target_simulator XSim [current_project]

# Add files
add_files -norecurse "Firmware.vhd Firmware_pkg.vhd Firmware_tb.vhd datafifo_dcfeb_top.vhd ../ip/$FPGA_TYPE/clockManager/clockManager.xci ../ip/$FPGA_TYPE/ila/ila.xci ../ip/$FPGA_TYPE/lut_input1/lut_input1.xci ../ip/$FPGA_TYPE/lut_input2/lut_input2.xci ../ip/$FPGA_TYPE/datafifo_dcfeb/datafifo_dcfeb.xci"
add_files -fileset constrs_1 -norecurse "Firmware_tb.xdc"

# Add tcl for simulation
set_property -name {xsim.simulate.custom_tcl} -value {../../../../source/Firmware_tb.tcl} -objects [get_filesets sim_1]

# Set ip as global
set_property generate_synth_checkpoint false [get_files  ../ip/$FPGA_TYPE/clockManager/clockManager.xci]
set_property generate_synth_checkpoint false [get_files  ../ip/$FPGA_TYPE/ila/ila.xci]
set_property generate_synth_checkpoint false [get_files  ../ip/$FPGA_TYPE/lut_input1/lut_input1.xci]
set_property generate_synth_checkpoint false [get_files  ../ip/$FPGA_TYPE/lut_input2/lut_input2.xci]
set_property generate_synth_checkpoint false [get_files  ../ip/$FPGA_TYPE/datafifo_dcfeb/datafifo_dcfeb.xci]

puts "\[Success\] Created tb_project"
close_project
