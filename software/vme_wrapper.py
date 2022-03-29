#!/bin/env python
from __future__ import print_function, division
if hasattr(__builtins__, 'raw_input'): input=raw_input

import os, sys, cmd, glob
import multiprocessing
import subprocess
import argparse
import time

# Environment setup constants

# OSU
osu = {
    'vme_slot' : '07',
    'vcc_mac_address' : '02:00:00:00:00:02',
    'eth_name' : 'eth1',
    'schar_port' : '1',
}

# UCSB
ucsb = {
    'vme_slot' : '19',
    'vcc_mac_address' : '02:00:00:00:00:4A',
    'eth_name' : 'p5p2',
    'schar_port' : '2',
}

# B904
b904_me11 = {
    'vme_slot' : '7',
    'vcc_mac_address' : '02:00:00:00:00:30',
    'eth_name' : 'p1p2',  # schar_port overwrites X in p1pX
    'schar_port' : '3',
}

b904_me21 = {
    'vme_slot' : '9',
    'vcc_mac_address' : '02:00:00:00:00:30',
    'eth_name' : 'p1p2',  # schar_port overwrites X in p1pX
    'schar_port' : '2',
}

vme_config = b904_me11

def run_vme_command(r_w, vme_cmd, vme_data='0000', comment='', show_ret=False, wait=0.01):
    r_w = r_w.lower()
    argv = ['./vme_cli', '--vme_write_read', r_w, '--vme_command', vme_cmd,]
    for k, v in vme_config.items():
        argv += ['--'+k, v]
    if r_w == 'w':
        argv += ['--vme_data', vme_data]

    ret = subprocess.check_output(argv)
    time.sleep(wait)
    if show_ret:
        print( '\n'.join(ret.split('\n')[4:-2]), '"{}"'.format(comment) )
    return ret.split('\n')[-3].split()[2]

def interactive():
    while True:
        cmd = input('>> ').split()
        if len(cmd) == 0:
            continue
        elif cmd[0] == 'exit' or cmd[0] == '.q':
            break
        elif cmd[0] == 'set':
            if (len(cmd) < 3):
                print( 'Missing config setting! Do: set <key> <value>' )
                continue
            if cmd[1] not in vme_config:
                print( 'Key \'{}\' is not found in the config list!'.format(cmd[1]) )
                continue
            prev = vme_config[cmd[1]]
            vme_config[cmd[1]] = cmd[2]
            print( 'Config variable \'{}\' is set to {}  (previous value is {}).'.format(cmd[1], cmd[2], prev))
        elif cmd[0] == 'run':
            if (len(cmd) < 2):
                print( 'Missing test name' )
                continue
            test_set(*cmd[1:])
        elif cmd[0].lower() == 'r':
            if (len(cmd) < 2):
                print( 'Missing VME command!' )
                continue
            res = run_vme_command('r', cmd[1], wait=0.0)
            print( '{}   {}    {} '.format(cmd[0].upper(), cmd[1].upper(), res))
        elif cmd[0].lower() == 'w':
            if (len(cmd) < 3):
                print( 'Missing VME data!' )
                continue
            res = run_vme_command('w', cmd[1], cmd[2], wait=0.0)
        else:
            print( 'Invalid command!' )
            continue


def test_set(test_name, cfeb_slot=None):
    if test_name=='dmbid':
        run_vme_command('w', '293C', '3C8')
        run_vme_command('w', '2F04', '0000')
        out1 = run_vme_command('r', '2014')
        run_vme_command('w', '2F08', '0000')
        out2 = run_vme_command('r', '2014')
        print(out1)
        print(out2)
        print('Test complete.')

    elif test_name=='odmbid':
        result = run_vme_command('r', '4100')
        print(result)
        print('Test complete.')

    elif test_name=='readcfg':
        result = ''
        for i in range(16):
            cmd = '{:x}'.format(0x4000 + (i<<2))
            result += '{0:>4X}'.format( int(run_vme_command('r', cmd),16) )
            result += '\n' if i % 4 == 3 else ' '
        print(result)

    elif test_name=='readconst':
        result = ''
        for i in range(1,16):
            cmd = '{:x}'.format(0x4000 + (i<<8))
            result += '{0:>4X}'.format( int(run_vme_command('r', cmd),16) )
            result += '\n' if i % 4 == 0 else ' '
        print(result)

    elif test_name=='checkprom':
        result = ''
        run_vme_command('W', '602C', '1D' )
        run_vme_command('W', '602C', '26' )
        result += run_vme_command('R', '6038')+'  ref: 8800\n'
        run_vme_command('W', '602C', '46' )
        result += run_vme_command('R', '6038')+'  ref: 8880\n'
        run_vme_command('W', '602C', '66' )
        result += run_vme_command('R', '6038')+'  ref: 88FF\n'
        run_vme_command('W', '602C', '86' )
        result += run_vme_command('R', '6038')+'  ref: 88FF\n'
        run_vme_command('W', '602C', 'A6' )
        result += run_vme_command('R', '6038')+'  ref: 88FB\n'
        run_vme_command('W', '602C', 'C6' )
        result += run_vme_command('R', '6038')+'  ref: 88FF\n'
        run_vme_command('W', '602C', '3D' )
        run_vme_command('W', '602C', '26' )
        result += run_vme_command('R', '6038')+'  ref: 8800\n'
        run_vme_command('W', '602C', '46' )
        result += run_vme_command('R', '6038')+'  ref: 8880\n'
        run_vme_command('W', '602C', '66' )
        result += run_vme_command('R', '6038')+'  ref: 88FF\n'
        run_vme_command('W', '602C', '86' )
        result += run_vme_command('R', '6038')+'  ref: 88FF\n'
        run_vme_command('W', '602C', 'A6' )
        result += run_vme_command('R', '6038')+'  ref: 88FB\n'
        run_vme_command('W', '602C', 'C6' )
        result += run_vme_command('R', '6038')+'  ref: 88FF\n'
        run_vme_command('W', '602C', '1D' )
        print(result)

    elif test_name=='dcfebid':
        result = ''
        run_vme_command('w', '1018', '0000')
        run_vme_command('w', '1020', cfeb_slot)
        run_vme_command('w', '191C', '03C8')
        run_vme_command('w', '1F04', '0000')
        result = run_vme_command('r', '1014')+result
        run_vme_command('w', '1F08', '0000')
        result = run_vme_command('r', '1014')+result
        print(result)
        print('Test complete.')

    elif test_name=='xdcfebid':
        if not cfeb_slot:
            print('Missing xDCFEB number, default to 1')
            cfeb_slot = '1'
        result = ''
        run_vme_command('w', '1018', '0000')
        run_vme_command('w', '1020', cfeb_slot)
        run_vme_command('w', '1934', '03C8')
        run_vme_command('w', '1F30', 'FFFF')
        run_vme_command('w', '1F30', 'FFFF')
        run_vme_command('w', '1F30', 'FFFF')
        run_vme_command('w', '1338', '000F')
        run_vme_command('w', '1F04', '0000')
        result = run_vme_command('r', '1014')+result
        run_vme_command('w', '1F08', '0000')
        result = run_vme_command('r', '1014')+result
        print(result)
        print('Test complete.')

    elif test_name=='xdcfebjtag':
        if not cfeb_slot:
            print('Missing xDCFEB number, default to 1')
            cfeb_slot = '1'
        result=''
        run_vme_command('w', '1018', '0000')
        run_vme_command('w', '1020', cfeb_slot)
        run_vme_command('w', '1934', '03C2')
        run_vme_command('w', '1F30', 'FFFF')
        run_vme_command('w', '1F30', 'FFFF')
        run_vme_command('w', '1F30', 'FFFF')
        run_vme_command('w', '1338', '000F')
        run_vme_command('w', '1704', '000C')
        run_vme_command('w', '1308', '0000')
        run_vme_command('w', '1934', '03C3')
        run_vme_command('w', '1F30', 'FFFF')
        run_vme_command('w', '1F30', 'FFFF')
        run_vme_command('w', '1F30', 'FFFF')
        run_vme_command('w', '1338', '000F')
        run_vme_command('w', '1B04', '0123')
        run_vme_command('w', '1308', '0000')
        run_vme_command('w', '1B04', '0456')
        run_vme_command('w', '1308', '0000')
        result=result+run_vme_command('r', '1014')
        run_vme_command('w', '1B04', '0789')
        run_vme_command('w', '1308', '0000')
        result=result+run_vme_command('r', '1014')
        run_vme_command('w', '1B04', '0ABC')
        run_vme_command('w', '1308', '0000')
        result=result+run_vme_command('r', '1014')
        run_vme_command('w', '1B04', '0DEF')
        run_vme_command('w', '1308', '0000')
        result=result+run_vme_command('r', '1014')
        if result == '123456789ABC':
            print('Success!', result)
        else:
            print('Failed: read {} instead of 123456789ABC'.format(result))
        print('Test complete.')

    elif test_name=='cfebsetup':
        run_vme_command('w', '8010', 'FF', 'Turn on all FEBs', show_ret=True, wait=1.0)
        run_vme_command('r', '3120', '', 'Read Done bits of all CFEBs', show_ret=True)

        run_vme_command('w', '3004', '1', 'Global reset', show_ret=True, wait=0.1)
        run_vme_command('w', '401C', '0', 'Set kill: None', show_ret=True)
        run_vme_command('w', '3014', '1', 'DCFEB resync -- reset all counters', show_ret=True, wait=0.1)
        run_vme_command('w', '3400', '1', 'Set Pedestal', show_ret=True)

        # run_vme_command('w', '3008', '1', 'Optical reset', show_ret=True, wait=0.1)


    elif test_name=='readpkt':
        res = []
        res.append( run_vme_command('r', '341C', '0', 'Received data packets of DCFEB1',  wait=0.001) )
        res.append( run_vme_command('r', '342C', '0', 'Received data packets of DCFEB2',  wait=0.001) )
        res.append( run_vme_command('r', '343C', '0', 'Received data packets of DCFEB3',  wait=0.001) )
        res.append( run_vme_command('r', '344C', '0', 'Received data packets of DCFEB4',  wait=0.001) )
        res.append( run_vme_command('r', '345C', '0', 'Received data packets of DCFEB5',  wait=0.001) )
        res.append( run_vme_command('r', '346C', '0', 'Received data packets of DCFEB6',  wait=0.001) )
        res.append( run_vme_command('r', '347C', '0', 'Received data packets of DCFEB7',  wait=0.001) )
        print(', '.join(res))

    elif test_name=='fiberlite':
        cfeb_num = 1
        run_vme_command('r', '3120', '', 'Read Done bits of all CFEBs', show_ret=True)
        run_vme_command('w', '401C', '0', 'Set kill: None', show_ret=True)
        run_vme_command('w', '3200', '4', 'Send test L1A(MATCH) to all DCFEBs', show_ret=True)
        res = []
        for cfeb_num in range(1, 8):
            res.append(run_vme_command('r', '34{}C'.format(cfeb_num), '', 'Read number of received packets for DCFEB{}'.format(cfeb_num)))
        print(', '.join(res))

    elif test_name=='fibertest':
        for cfeb_num in range(1, 8):
            cfeb_slot = 1 << (cfeb_num - 1)
            killstr = '{}'.format(hex(0x7F & ~cfeb_slot))  
            # run_vme_command('r', '3120', '', 'Read Done bits of all CFEBs', show_ret=True)
            # run_vme_command('r', '3120', '', 'Read Done bits of all CFEBs', show_ret=True)
            # run_vme_command('w', '3200', '4', 'Read Done bits of all CFEBs', show_ret=True)
            # run_vme_command('r', '34{}C'.format(cfeb_num), '', 'Read number of received packets', show_ret=True)

    elif test_name=='otmbprbs':
        vme_config['vme_slot'] = str( int(vme_config['vme_slot']) - 1) 
        print('Set vme_slot to: ', vme_config['vme_slot'])
        run_vme_command('w', '01EE',   '1', 'Set OTMB to ODMB mode', show_ret=True, wait=0.1)
        run_vme_command('w', '01EE',   '1', 'Set OTMB to ODMB mode again', show_ret=True, wait=0.1)
        run_vme_command('r', '01EE',    '', 'Read ODMB mode', show_ret=True, wait=0.1)
        run_vme_command('w', '31EE', '100', 'Start OTMB PRBS sequence', show_ret=True, wait=0.5)

        vme_config['vme_slot'] = str( int(vme_config['vme_slot']) + 1) 
        print('Set vme_slot to: ', vme_config['vme_slot'])
        run_vme_command('r', '9408', '', 'Read number of PRBS matches', show_ret=True, wait=0.5)
        run_vme_command('r', '940C', '', 'Read number of PRBS errors', show_ret=True, wait=0.5)

    elif test_name=='discretelogic':
        #V6 3C8 read user code
        #dl_pattern = '111110110000020022231010'
        #V6 3C9 read ID code
        #dl_pattern = '111110110020020022231010'
        #KUS 09 read ID code
        dl_pattern = '11111011002002011010'
        #KUS 08 read user code
        #dl_pattern = '11111011000002011010'
        for char in dl_pattern:
            run_vme_command('w', 'FFFC', '000'+char)
            result = ''
        for hex_digit in range(8):
            for command in range(8):
                if hex_digit==0:
                    if command%2==1:
                        result = run_vme_command('r', 'FFFC')+result
                    elif command==0:
                        run_vme_command('w', 'FFFC', '0000')
                    else:
                        run_vme_command('w', 'FFFC', '0002')
                elif hex_digit==1:
                    if command%2==1:
                        result = run_vme_command('r', 'FFFC')+result
                    elif command==0:
                        run_vme_command('w', 'FFFC', '0002')
                    else:
                        run_vme_command('w', 'FFFC', '0000')
                else:
                    if command%2==1:
                        result = run_vme_command('r', 'FFFC')+result
                    else:
                        run_vme_command('w', 'FFFC', '0000')
                        print(result)
                        print(hex(int(result,2)))
                        print('Test complete.')
    else:
         print('Do not find test {}!'.format(test_name))


if __name__=='__main__':

    parser = argparse.ArgumentParser(description='Wrapper for vme_cli', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-t', '--test', default='interact', help='Sets VME commands to be run.')
    parser.add_argument('-c', '--cfeb', default='0001', choices=['0001', '0002', '0004', '0008', '0010', '0020', '0040'], help='Sets cfeb slot.')
    
    args = vars(parser.parse_args())
    test_name = args['test']
    
    if test_name == 'interact':
        interactive()
    else:
        test_set(test_name, args['cfeb'])

