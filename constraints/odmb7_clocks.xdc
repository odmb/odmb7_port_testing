# ODMB7 Input Clock Constraints XDC file
# ----------------------------------------------------------------------------------------------------------------------

# ----------------------------------
# Create system clock groups
# ----------------------------------
create_clock -name cms_clk  -period 25    [get_ports CMS_CLK_FPGA_P]
create_clock -name gp_clk_6 -period 12.5  [get_ports GP_CLK_6_P]
create_clock -name gp_clk_7 -period 12.5  [get_ports GP_CLK_7_P]
create_clock -name lf_clk -period 100000  [get_ports LF_CLK]
set_clock_groups -group [get_clocks cms_clk -include_generated_clocks] -asynchronous
set_clock_groups -group [get_clocks gp_clk_6 -include_generated_clocks] -asynchronous
set_clock_groups -group [get_clocks gp_clk_7 -include_generated_clocks] -asynchronous
set_clock_groups -group [get_clocks lf_clk -include_generated_clocks] -asynchronous
set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets {LF_CLK_IBUF_inst/O}]

# ----------------------------------
# Debug core configs
# ----------------------------------
# set_property C_CLK_INPUT_FREQ_HZ 80000000 [get_debug_cores dbg_hub]
# set_property C_ENABLE_CLK_DIVIDER true [get_debug_cores dbg_hub]

# # ----------------------------------
# # Set dedicated clock pin false
# # ----------------------------------
# set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets CCB_CMD_S]
# set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets CCB_DATA_S]
# set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets CCB_BX0_B]
# set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets CCB_BX_RST_B]
# set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets CCB_CLKEN]
# set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets CCB_EVCNTRES]

# ----------------------------------
# Create the MGT reference clocks
# ----------------------------------
create_clock -name gth_refclk0_q224 -period 6.25 [get_ports REF_CLK_1_P]
create_clock -name gth_refclk0_q225 -period 8.33 [get_ports REF_CLK_4_P]
create_clock -name gth_refclk0_q226 -period 6.4  [get_ports REF_CLK_3_P]
create_clock -name gth_refclk1_q226 -period 8    [get_ports CLK_125_REF_P]
create_clock -name gth_refclk0_q227 -period 6.25 [get_ports REF_CLK_2_P]
create_clock -name gth_refclk1_q227 -period 8.33 [get_ports REF_CLK_5_P]
set_clock_groups -group [get_clocks gth_refclk0_q224 -include_generated_clocks] -asynchronous
set_clock_groups -group [get_clocks gth_refclk0_q225 -include_generated_clocks] -asynchronous
set_clock_groups -group [get_clocks gth_refclk0_q226 -include_generated_clocks] -asynchronous
set_clock_groups -group [get_clocks gth_refclk1_q226 -include_generated_clocks] -asynchronous
set_clock_groups -group [get_clocks gth_refclk0_q227 -include_generated_clocks] -asynchronous
set_clock_groups -group [get_clocks gth_refclk1_q227 -include_generated_clocks] -asynchronous

# False path constraints
# ----------------------------------------------------------------------------------------------------------------------
set_false_path -to [get_cells -hierarchical -filter {NAME =~ *bit_synchronizer*inst/i_in_meta_reg}] -quiet
##set_false_path -to [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_*_reg}] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*D} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_meta*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*PRE} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_meta*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*PRE} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_sync1*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*PRE} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_sync2*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*PRE} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_sync3*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*PRE} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_out*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*CLR} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_meta*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*CLR} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_sync1*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*CLR} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_sync2*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*CLR} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_sync3*}]] -quiet
set_false_path -to [get_pins -filter {REF_PIN_NAME=~*CLR} -of_objects [get_cells -hierarchical -filter {NAME =~ *reset_synchronizer*inst/rst_in_out*}]] -quiet

set_false_path -to [get_cells -hierarchical -filter {NAME =~ *gtwiz_userclk_tx_inst/*gtwiz_userclk_tx_active_*_reg}] -quiet
set_false_path -to [get_cells -hierarchical -filter {NAME =~ *gtwiz_userclk_rx_inst/*gtwiz_userclk_rx_active_*_reg}] -quiet

