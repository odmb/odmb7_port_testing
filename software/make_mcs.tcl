# Usage: vivado -nojournal -nolog -mode batch -source make_mcs.tcl -tclargs [version] [outname] [5|7]
#   Example: vivado_source make_mcs.tcl -tclargs v20_test 5
#   Example: vivado_source make_mcs.tcl -tclargs v20_test cfeb_test odmb5

set BOARD odmb7
set INFILE "../project/odmb7_ucsb_dev.runs/impl_1/odmb7_ucsb_dev.bit" 
set INFILE5 "../project/odmb5_ucsb_dev.runs/impl_3/odmb5_ucsb_dev.bit" 
set OUTNAME "ucsb_dev" 
set OUTDIR "../firmware"

set curtime [string map {\" {}} [exec date +"%y%m%d-%H%M"]]
set VERSION ver-$curtime

if { $argc > 0 } {
    set BOARD [lindex $argv end]
    if { $BOARD == 7 } { set BOARD odmb7 }
    if { $BOARD == 5 } { set BOARD odmb5 }
}

if { $BOARD != "odmb7" && $BOARD != "odmb5" } {
    puts "Invalid Board: $BOARD !!"
    return 1
}

if { $BOARD == "odmb5" } {
    set INFILE $INFILE5
}

if { $argc > 1 } {
    set VERSION [lindex $argv 0]
    if { $argc > 2 } {
        set OUTNAME [lindex $argv 1]
    } else {
        set OUTNAME $VERSION
    }
}

set OUTFILE "${OUTDIR}/${VERSION}/${BOARD}_${OUTNAME}.mcs"

if { ![file exist $INFILE] } {
    puts "Input file: $INFILE does not exist!!"
    return 1
} else {
    exec mkdir -p ${OUTDIR}/${VERSION}
    exec cp $INFILE ${OUTDIR}/${VERSION}/${BOARD}_${OUTNAME}.bit
    # exec cp [string map {.bit .bin} $INFILE] ${OUTDIR}/${VERSION}/${BOARD}_${OUTNAME}.bin
    exec cp [string map {.bit .ltx} $INFILE] ${OUTDIR}/${VERSION}/${BOARD}_${OUTNAME}.ltx

    write_cfgmem -format mcs -interface SPIx8 -size 32 -loadbit "up 0 $INFILE" -file "$OUTFILE"
    exec rm ${OUTDIR}/${VERSION}/${BOARD}_${OUTNAME}_primary.prm
    exec rm ${OUTDIR}/${VERSION}/${BOARD}_${OUTNAME}_secondary.prm
}
