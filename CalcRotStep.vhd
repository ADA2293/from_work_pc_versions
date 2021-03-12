----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    16:01:25 01/09/2013 
-- Design Name: 
-- Module Name:    RotStep - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity CalcRotStep is
	generic
		(
		Bits:						natural := 32			--> für Variable Bitbreiten bis 32
		);
	Port
		(
		Rst :											in		STD_LOGIC;
		Clk :											in		STD_LOGIC;
		NewDin :										in		STD_LOGIC;
		PhiStep:										in		STD_LOGIC_VECTOR(Bits -1 downto 0);	--> FIX_(Bits)_(Bits-4); Phasen Schrittweite
		RotPhi:										out	STD_LOGIC_VECTOR(Bits -1 downto 0);	--> FIX_(Bits)_(Bits-4); Ergebniss
		NewDout :									out	STD_LOGIC
--> #########   DEBUG  ########### 
		;CrsDsp_Val1:								out 	STD_LOGIC_VECTOR (31 downto 0);
		CrsDsp_Val2:								out 	STD_LOGIC_VECTOR (31 downto 0);
		CrsDsp_DBG0:								out	STD_LOGIC;
		CrsDsp_DBG1:								out	STD_LOGIC;
		CrsDsp_DBG2:								out	STD_LOGIC;
		ScrDsp_DBG3:								out	STD_LOGIC
--> ############################## 
		);
end CalcRotStep;

architecture Behavioral of CalcRotStep is
	--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	--<<<< Signal Declaration
	--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	--> Signale für Vetor Rotation
--	constant	ConstHalfPiPlus:					STD_LOGIC_VECTOR (32 downto 0)		:= 	'0' & x"3243F6A9";
--	constant	ConstHalfPiMinus:					STD_LOGIC_VECTOR (32 downto 0)		:= 	'1' & x"CDBC0957";
	constant	ConstPiPlus:						STD_LOGIC_VECTOR (32 downto 0)		:=		'0' & x"6487ED51";
	constant	ConstPiMinus:						STD_LOGIC_VECTOR (32 downto 0)		:=		'1' & x"9B7812AF";
	constant	Const2PiPlus:						STD_LOGIC_VECTOR (32 downto 0)		:=		'0' & x"C90FDAA2";

	signal ActPhiStep:							STD_LOGIC_VECTOR(Bits -1 downto 0)	:= (OTHERS => '0');
	signal CmpPhiStep:							STD_LOGIC_VECTOR(Bits -1 downto 0)	:= (OTHERS => '0');	--> wir verwenden dies wg. der besseren Performance bei eingeschaltetem Resource Sharing
	signal ActPhiStep_1:							STD_LOGIC_VECTOR(Bits -1 downto 0)	:= (OTHERS => '0');
	signal State:									STD_LOGIC_VECTOR(1 downto 0)			:= "00";

begin
--CrsDsp_DBG0 <= State(0);
--CrsDsp_DBG1 <= State(1);
--CrsDsp_DBG2 <= State(2);
--CrsDsp_Val1 <= ActPhiStep;
--CrsDsp_Val2 <= PlusOvfl;
----------------------------------------------------------------------------------
--> calculate next RotPhi
--> 3Clk's latency
--!!!!!!!!! Resource Sharing abschalten !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--
--> Max. Clk Erhöhung von 93MHz auf 134MHz möglich
----------------------------------------------------------------------------------
	CalcRotStepProc: process(Clk)
		begin
			if rising_edge(Clk) then
				NewDout <=							'0';

				if Rst = '1' then
					ActPhiStep <=					(OTHERS => '0');							--> start with 0
					CmpPhiStep <=					(OTHERS => '0');							--> start with 0
					ActPhiStep_1 <=				(OTHERS => '0');							--> start with 0
					State <=							(OTHERS => '0');
					RotPhi <=						(OTHERS => '0');
					NewDout <=	'0';
				
				elsif NewDin = '1' then
--> 1.Clk
					ActPhiStep <=					ActPhiStep_1 + PhiStep;
					CmpPhiStep <=					ActPhiStep_1 + PhiStep + (ConstPiPlus(32 downto (32 - (Bits - 1))) + ConstPiPlus(32 - Bits));
					State <=							State(0) & '1';
				else
					State <=							State(0) & '0';
				end if;
				
				if State(0) = '1' then
--> 2.Clk
--					if ActPhiStep > (ConstPiPlus(32 downto (32 - (Bits - 1))) + ConstPiPlus(32 - Bits)) then
--						ActPhiStep_1 <= ActPhiStep - (Const2PiPlus(32 downto (32 - (Bits - 1))) + Const2PiPlus(32 - Bits));
--					elsif ActPhiStep < (ConstPiMinus(32 downto (32 - (Bits - 1))) + ConstPiMinus(32 - Bits)) then
--						ActPhiStep_1 <= ActPhiStep + (Const2PiPlus(32 downto (32 - (Bits - 1))) + Const2PiPlus(32 - Bits));
--					else
--						ActPhiStep_1 <= ActPhiStep;
--					end if;

					--> wir verwenden dies wg. der besseren Performance bei eingeschaltetem Resource Sharing
					if CmpPhiStep(Bits-1) = '1' then
						ActPhiStep_1 <= ActPhiStep + (Const2PiPlus(32 downto (32 - (Bits - 1))) + Const2PiPlus(32 - Bits));
					elsif CmpPhiStep > (Const2PiPlus(32 downto (32 - (Bits - 1))) + Const2PiPlus(32 - Bits)) then
						ActPhiStep_1 <= ActPhiStep - (Const2PiPlus(32 downto (32 - (Bits - 1))) + Const2PiPlus(32 - Bits));
					else
						ActPhiStep_1 <= ActPhiStep;
					end if;
				end if;
				
				if State(1) = '1' then
--> 3.Clk
					RotPhi <=						ActPhiStep_1;
					NewDout <=						'1';
				end if;
			end if; -- if rising_edge(Clk)
		end process CalcRotStepProc;
end Behavioral;

