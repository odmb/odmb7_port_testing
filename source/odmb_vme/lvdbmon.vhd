-- LVDBMON: Monitors voltages in LVMB and powers on/off DCFEBs+ALCT

library ieee;
library work;
library unisim;
--library hdlmacro;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ucsb_types.all;
use work.Latches_Flipflops.all;
use unisim.vcomponents.all;
--use hdlmacro.hdlmacro.all;

entity LVDBMON is  
  port (
    CSP_LVMB_LA_CTRL : inout std_logic_vector(35 downto 0);

    SLOWCLK   : in std_logic;
    RST       : in std_logic;
    PON_RESET : in std_logic;           -- Power on reset

    DEVICE  : in std_logic;
    STROBE  : in std_logic;
    COMMAND : in std_logic_vector(9 downto 0);
    WRITER  : in std_logic;

    INDATA  : in  std_logic_vector(15 downto 0);
    OUTDATA : out std_logic_vector(15 downto 0);

    DTACK : out std_logic;

    LVADCEN : out std_logic_vector(6 downto 0);
    ADCCLK  : out std_logic;
    ADCDATA : out std_logic;
    ADCIN   : in  std_logic;

    LVTURNON   : out std_logic_vector(8 downto 1);
    R_LVTURNON : in  std_logic_vector(8 downto 1);
    LOADON     : out std_logic;
    DIAGOUT    : out std_logic_vector(17 downto 0)
    );
end LVDBMON;

architecture LVDBMON_Arch of LVDBMON is

  signal BUSY                                           : std_logic;
  signal WRITEADC, READMON, WRITEPOWER, READPOWER       : std_logic;
  signal READPOWERSTATUS, SELADC, READADC               : std_logic;
  signal SELADC_vector                                  : std_logic_vector(3 downto 1);
  signal LVTURNON_INNER                                 : std_logic_vector(8 downto 1);
  signal D_OUTDATA, Q_OUTDATA, D_OUTDATA_2, Q_OUTDATA_2 : std_logic;
  signal D_DTACK_2, Q_DTACK_2, D_DTACK_4, Q_DTACK_4     : std_logic;
  signal C_LOADON, Q1_LOADON, Q2_LOADON                 : std_logic;
  signal LOADON_INNER, ADCCLK_INNER                     : std_logic;
  signal CE_ADCCLK, CLR_ADCCLK                          : std_logic;
  signal RSTBUSY, CLKMON                                : std_logic;
  signal CE1_BUSY, CE2_BUSY, CLR_BUSY                   : std_logic;
  signal Q1_BUSY, Q2_BUSY, D_BUSY, DONEMON, LOAD        : std_logic;
  signal blank1, blank2                                 : std_logic;
  signal QTIME                                          : std_logic_vector(7 downto 0);
  signal CLR1_LOAD, CLR2_LOAD, Q1_LOAD, Q2_LOAD         : std_logic;
  signal Q3_LOAD, Q4_LOAD, CE_LOAD, ASYNLOAD            : std_logic;
  signal RDMONBK                                        : std_logic;
  signal CE_OUTDATA_FULL                                : std_logic;
  signal Q_OUTDATA_FULL                                 : std_logic_vector(15 downto 0);
  signal SLI_ADCDATA, L_ADCDATA, CE_ADCDATA             : std_logic;
  signal Q_ADCDATA                                      : std_logic_vector(7 downto 0);

  signal cmddev : std_logic_vector (15 downto 0);

  signal diaglvdb_inner   : std_logic_vector (17 downto 0);
  
begin  --Architecture

-- Decode instruction
  cmddev <= "000" & DEVICE & COMMAND & "00";

  WRITEADC        <= '1' when (CMDDEV = x"1000") else '0';
  READMON         <= '1' when (CMDDEV = x"1004") else '0';
  WRITEPOWER      <= '1' when (CMDDEV = x"1010") else '0';
  READPOWER       <= '1' when (CMDDEV = x"1014") else '0';
  READPOWERSTATUS <= '1' when (CMDDEV = x"1018") else '0';
  SELADC          <= '1' when (CMDDEV = x"1020") else '0';
  READADC         <= '1' when (CMDDEV = x"1024") else '0';

-- Generate OUTDATA
  FDCE_GEN : for i in 0 to 2 generate
  begin
    FDCE_OUT : FDCE port map (Q => SELADC_vector(i+1), C => STROBE, CE => SELADC, CLR => RST, D => INDATA(i));
  end generate FDCE_GEN;

  OUTDATA <= '0' & x"000" & SELADC_vector(3 downto 1) when (STROBE = '1' and READADC = '1') else (others => 'Z');

  D_OUTDATA <= '1' when (STROBE = '1' and READADC = '1') else '0';
  FD_OUTDATA : FD port map (Q => Q_OUTDATA, C => SLOWCLK, D => D_OUTDATA);

-- Generate DTACK_2
  D_DTACK_2 <= SELADC and STROBE;
  FD_DTACK_2 : FD port map (Q => Q_DTACK_2, C => SLOWCLK, D => D_DTACK_2);

-- Generate OUTDATA_2
  FDCE_GEN2 : for i in 0 to 7 generate
  begin
    --MO: order is a bit strange, didn't check yet if this makes sense
    FDPE_OUT2 : FDPE port map (Q => LVTURNON_INNER(i+1), C => STROBE, CE => WRITEPOWER, D => INDATA(i), PRE => RST);
  end generate FDCE_GEN2;

  OUTDATA <= x"00" & LVTURNON_INNER(8 downto 1) when (STROBE = '1' and READPOWER = '1') else
             x"00" & R_LVTURNON(8 downto 1) when (STROBE = '1' and READPOWERSTATUS = '1') else
             (others => 'Z');
  D_OUTDATA_2 <= '1' when (STROBE = '1' and READPOWER = '1') else
                 '1' when (STROBE = '1' and READPOWERSTATUS = '1') else
                 '0';

  FD_OUTDATA_2 : FD port map (Q => Q_OUTDATA_2, C => SLOWCLK, D => D_OUTDATA_2);

-- Generate DTACK_4
  D_DTACK_4 <= '1' when (WRITEPOWER = '1' and STROBE = '1') else '0';
  FD_DTACK_4 : FD port map (Q => Q_DTACK_4, C => SLOWCLK, D => D_DTACK_4);

-- Generate RDMONBK
  RDMONBK <= '1' when (READMON = '1' and STROBE = '1' and BUSY = '0') else '0';

-- Generate LVADCEN
  LVADCEN(0) <= '0' when SELADC_vector(3 downto 1) = "000" else '1';
  LVADCEN(1) <= '0' when SELADC_vector(3 downto 1) = "001" else '1';
  LVADCEN(2) <= '0' when SELADC_vector(3 downto 1) = "010" else '1';
  LVADCEN(3) <= '0' when SELADC_vector(3 downto 1) = "011" else '1';
  LVADCEN(4) <= '0' when SELADC_vector(3 downto 1) = "100" else '1';
  LVADCEN(5) <= '0' when SELADC_vector(3 downto 1) = "101" else '1';
  LVADCEN(6) <= '0' when SELADC_vector(3 downto 1) = "110" else '1';

-- Generate LOADON: from VME command and from Power-on reset
  --pon_reset_b <= not pon_reset;
  --FDPON      : FD port map(pon_reset_b1, slowclk, pon_reset_b);
  --PULSEPON   : PULSE2SAME port map(pon_pulse, slowclk, rst, pon_reset_b1);
  --C_LOADON    <= (WRITEPOWER and STROBE) or pon_pulse;
  C_LOADON    <= (WRITEPOWER and STROBE);
  FDC_LOADON : FDC port map (Q => Q1_LOADON, C => C_LOADON, CLR => LOADON_INNER, D => '1');
  FD_LOADON1 : FD port map (Q => Q2_LOADON, C => SLOWCLK, D => Q1_LOADON);
  FD_LOADON2 : FD port map (Q => LOADON_INNER, C => SLOWCLK, D => Q2_LOADON);

-- Generate OUTDATA
  CE_OUTDATA_FULL      <= '1'                         when (BUSY = '1' and RSTBUSY = '0' and CLKMON = '0') else '0';
  --MO: switch from hdlmacro to latches_flipflops for now
  --SR16CE_OUTDATA : SR16CE port map (Q => Q_OUTDATA_FULL, C => SLOWCLK, CE => CE_OUTDATA_FULL, CLR => RST, SLI => ADCIN);
  SR16CE(SLOWCLK, CE_OUTDATA_FULL, RST, ADCIN, Q_OUTDATA_FULL, Q_OUTDATA_FULL);
  OUTDATA(15 downto 0) <= Q_OUTDATA_FULL(15 downto 0) when (RDMONBK = '1') else
                          (others => 'Z');
  SLI_ADCDATA <= 'L';


-- Generate ADCDATA
  L_ADCDATA  <= '1' when (LOAD = '1' and CLKMON = '0') else '0';
  CE_ADCDATA <= '1' when (BUSY = '1' and CLKMON = '0') else '0';
  --MO: switch from hdlmacro to latches_flipflops for now
  --SR8CLE_ADCDATA : SR8CLE port map (Q => Q_ADCDATA, C => SLOWCLK, CE => CE_ADCDATA, CLR => RST, D => INDATA(7 downto 0), L => L_ADCDATA, SLI => SLI_ADCDATA);
  SR8CLE(SLOWCLK, CE_ADCDATA, RST, L_ADCDATA, SLI_ADCDATA, INDATA(7 downto 0), Q_ADCDATA, Q_ADCDATA);
  ADCDATA    <= Q_ADCDATA(7);

-- Generate ADCCLK
  CE_ADCCLK    <= '1' when (BUSY = '1' and RSTBUSY = '0') else '0';
  CLR_ADCCLK   <= '1' when (BUSY = '0' or RST = '1')      else '0';
  FDCE_ADCCLK : FDCE port map (Q => CLKMON, C => SLOWCLK, CE => CE_ADCCLK, CLR => CLR_ADCCLK, D => ADCCLK_INNER);
  ADCCLK_INNER <= not CLKMON;

-- Generate BUSY
  CE1_BUSY <= '1' when (BUSY = '1' and CLKMON = '0')                          else '0';
  CLR_BUSY <= Q2_BUSY or RST;
  --MO: switch from hdlmacro to latches_flipflops for now
  --CB8CE_BUSY : CB8CE port map (CEO => blank1, Q => QTIME, TC => blank2, C => SLOWCLK, CE => CE1_BUSY, CLR => CLR_BUSY);
  CB8CE(SLOWCLK, CE1_BUSY, CLR_BUSY, QTIME, QTIME, blank1, blank2);
  DONEMON  <= '1' when (QTIME(4) = '1' and QTIME(3) = '1' and QTIME(1) = '1') else '0';
  CE2_BUSY <= BUSY and CLKMON;
  FDCE_BUSY  : FDCE port map (Q => Q1_BUSY, C => SLOWCLK, CE => CE2_BUSY, CLR => CLR_BUSY, D => DONEMON);
  FD_BUSY    : FD port map(Q => Q2_BUSY, C => SLOWCLK, D => Q1_BUSY);
  RSTBUSY  <= RST or Q1_BUSY;
  D_BUSY   <= LOAD or BUSY;
  FDR_BUSY   : FDR port map (Q => BUSY, C => SLOWCLK, D => D_BUSY, R => RSTBUSY);

-- Generate LOAD
  ASYNLOAD  <= '1' when (STROBE = '1' and WRITEADC = '1' and BUSY = '0') else '0';
  CLR1_LOAD <= RST or Q2_LOAD;
  FDC_VCC    : FDC port map (Q => Q1_LOAD, C => ASYNLOAD, CLR => CLR1_LOAD, D => '1');
  FDC_LOAD1  : FDC port map (Q => LOAD, C => SLOWCLK, CLR => RST, D => Q1_LOAD);
  CE_LOAD   <= '1' when (BUSY = '1' and CLKMON = '0')                    else '0';
  FDCE_LOAD2 : FDCE port map (Q => Q2_LOAD, C => SLOWCLK, CE => CE_LOAD, CLR => RST, D => LOAD);
  FDC_LOAD3  : FDC port map (Q => Q3_LOAD, C => SLOWCLK, CLR => RST, D => Q2_LOAD);

  CLR2_LOAD <= '1' when (RST = '1' or WRITEADC = '0' or BUSY = '0') else '0';
  FDC_LOAD4 : FDC port map (Q => Q4_LOAD, C => Q3_LOAD, CLR => CLR2_LOAD, D => '1');

-- Generate LOADON / Generate DTACK / Generate LVTURNON / Generate ADCLK
  -- V2 default low, V3 default high
  LOADON <= not LOADON_INNER;
  LVTURNON <= LVTURNON_INNER;
  ADCCLK   <= ADCCLK_INNER;

  DTACK <= Q_OUTDATA or Q_DTACK_2 or Q_OUTDATA_2 or Q_DTACK_4 or RDMONBK or Q4_LOAD;
  
  DIAGOUT <= "000" & LOAD & CE1_BUSY & CLKMON & BUSY & Q1_BUSY & Q2_BUSY & RST & QTIME;

end LVDBMON_Arch;
