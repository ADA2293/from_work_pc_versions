----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    13:38:02 06/20/2011 
-- Design Name: 
-- Module Name:    Rect2Polar_S16 - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
-- This design implements a rectangular-to-polar coordinate conversion using a fully parallel CORDIC  (COordinate Rotation 
-- DIgital Computer) algorithm in vectoring mode. That is, given a complex-input <x,y>, it computes a new vector <m,a>, where 
-- magnitude m = K sqrt(x^2 + y^2), and the angle a = arctan(y/x).
-- !! As is common, the magnitude scale factor K = 1,646760... (for 8 stages) is not compensated in the processor!!
--
-- The CORDIC algorithm is implemented in 3 steps.
-- Step 1: Coarse Angle Rotation.  The algorithm converges only for angles between -pi/2 and pi/2, so if x < zero, the input vector is reflected 
--         to the 1st or 3rd quadrant by making the x-coordinate non-negative. 
-- Step 2: Fine Angle Rotation.  For rectangular-to-polar conversion, the resulting vector is rotated through progressively smaller angles, such that y goes
--         to zero. In the i-th stage, the angular rotation is by either +/- atan(1/2^i), depending on whether or not its input y is less than or greater than zero. 
-- Step 3: Angle Correction. If there was a reflection applied in Step 1, this step applies the appropriate angle correction by subtracting it from +/- pi.  
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
--
-- Additional Comments: 
-- wir verwenden als Zahlenformat Fix_16_12 -> Bit[15] Vorzeichen; Bit[14..12] Ganzahl; Bit[11..0] Fractional
--> mit den 12 Bit Fractional erreichen wir eine Genauigkeit von Mag: < 1.2*10^-3; Angel: < 4.9*10^-3
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity Rect2Polar_Generic is
	generic
		(
		Bits:						natural := 32			--> für Variable Input Bitbreiten bis 32
		);

	Port
		( 
		Rst :						in		STD_LOGIC;
		Clk :						in		STD_LOGIC;
		NewDin :					in		STD_LOGIC;
		x_in :					in		STD_LOGIC_VECTOR (Bits - 1 downto 0);
		y_in :					in		STD_LOGIC_VECTOR (Bits - 1 downto 0);
--		x_out :					out	STD_LOGIC_VECTOR (Bits - 1 downto 0);
--		y_out :					out	STD_LOGIC_VECTOR (Bits - 1 downto 0);
		mag_out :				out	STD_LOGIC_VECTOR (Bits - 1 downto 0);
		phase_out :				out	STD_LOGIC_VECTOR (Bits - 1 downto 0);
		NewDout:					out	STD_LOGIC
		);
end Rect2Polar_Generic;

architecture Behavioral of Rect2Polar_Generic is
--	--> zur verzögerten Durchleitung der Orginal x/y Werte
--type	T_XYBuff is array(integer range 0 to Bits - 3) of	STD_LOGIC_VECTOR (Bits - 1 downto 0); 
--	signal	XBuff:						T_XYBuff											:= (OTHERS =>(OTHERS => '0'));
--	signal	YBuff:						T_XYBuff											:= (OTHERS =>(OTHERS => '0'));

	--> Signale für QuadrantMap
	signal	QM_X_sign:					STD_LOGIC										:= '0';
	signal	QM_Y_sign:					STD_LOGIC										:= '0';
	signal	MR_X_sign:					STD_LOGIC_VECTOR (Bits - 3 downto 0)	:=(OTHERS => '0');
	signal	MR_Y_sign:					STD_LOGIC_VECTOR (Bits - 3 downto 0)	:=(OTHERS => '0');
	signal	In2Out:						STD_LOGIC_VECTOR (Bits - 1 downto 0)	:=(OTHERS => '0'); --> verzögertes NewDin für NewDout

	--> Signale zum Buffern der Eingangssignale		--> X/Y/Z(0)
	--> Signale zum Buffern von Quadrant Map			--> X/Y/Z(1)
	--> Signale für MicroRotation 						--> X/Y/Z(2..15)
type MRotTableType is array( integer range 0 to Bits - 1) of	STD_LOGIC_VECTOR(Bits -1 downto 0);
	signal	X:								MRotTableType									:= (OTHERS =>(OTHERS => '0'));
	signal	Y:								MRotTableType									:= (OTHERS =>(OTHERS => '0'));
	signal	Z:								MRotTableType									:= (OTHERS =>(OTHERS => '0'));


------------------
--> KONSTANTEN  --
------------------
--type AtanTableType is array( integer range 0 to 29) of	STD_LOGIC_VECTOR(31 downto 0);
--	constant AtanTable:					AtanTableType			:=	(	x"0C90FDAA", x"076B19C1", x"03EB6EBF",	x"01FD5BAA",
--																											x"00FFAADE", x"007FF557", x"003FFEAB", x"001FFFD5", 
--																											x"000FFFFB", x"0007FFFF", x"00040000",	x"00020000",
--																											x"00010000", x"00008000", x"00004000", x"00002000",
--																											x"00001000", x"00000800", x"00000400", x"00000200",
--																											x"00000100", x"00000080", x"00000040", x"00000020",
--																											x"00000010", x"00000008", x"00000004", x"00000002",
--																											x"00000001", x"00000000");
--	constant	ConstPiPlus:				STD_LOGIC_VECTOR (31 downto 0)			:= 	x"3243F6A9";
--	constant	ConstPiMinus:				STD_LOGIC_VECTOR (31 downto 0)			:= 	x"CDBC0957";
--> ein Bit mehr damit wir auch bei Bits = 32 noch runden können
type AtanTableType is array( integer range 0 to 29) of	STD_LOGIC_VECTOR(32 downto 0);
	constant AtanTable:					AtanTableType			:=	(	'0' & x"1921FB54", '0' & x"0ED63383", '0' & x"07D6DD7E", '0' & x"03FAB753",
																											'0' & x"01FF55BB", '0' & x"00FFEAAE", '0' & x"007FFD55", '0' & x"003FFFAB",
																											'0' & x"001FFFF5", '0' & x"000FFFFF", '0' & x"00080000", '0' & x"00040000",
																											'0' & x"00020000", '0' & x"00010000", '0' & x"00008000", '0' & x"00004000",
																											'0' & x"00002000", '0' & x"00001000", '0' & x"00000800", '0' & x"00000400",
																											'0' & x"00000200", '0' & x"00000100", '0' & x"00000080", '0' & x"00000040",
																											'0' & x"00000020", '0' & x"00000010", '0' & x"00000008", '0' & x"00000004",
																											'0' & x"00000002", '0' & x"00000001");
	constant	ConstPiPlus:				STD_LOGIC_VECTOR (32 downto 0)			:=		'0' & x"6487ED51"; --> Format Q33.29 (1Bit- Sign; 3Bits- integer; 29Bits fractional
	constant	ConstPiMinus:				STD_LOGIC_VECTOR (32 downto 0)			:=		'1' & x"9B7812AF"; --> Format Q33.29 (1Bit- Sign; 3Bits- integer; 29Bits fractional
---------------------------------------------------------------------------------'-----------------------------------

begin
---------------------------------------------------------------------------------------------
--> !!!!!!! wg. loop muß alles in einen Process !!!!!!
--> QuadrantMap + MicroRotation + QuadrantCorrect
---------------------------------------------------------------------------------------------
CordicVectoringKernel: process(Clk,Rst)
	begin
		if rising_edge(Clk) then
---------------------------------------------------------------------------------------------------------------
--			--> zur verzögerten Durchleitung der Orginal x/y Werte
--			for N in Bits - 2 downto 0 loop
--				--> shift Input to Output Clk by Clk
--				XBuff(N + 1) <=	XBuff(N);
--				YBuff(N + 1) <=	YBuff(N);
--			end loop;
--			if NewDin = '1' then
--				XBuff(0) <=				x_in;--(15 downto 1) & '1';	--> damit verhindern wir das x oder y NULL wird 
--				YBuff(0) <=				y_in;--(15 downto 1) & '1';	--> damit verhindern wir das x oder y NULL wird 
--			end if;
--			x_out <=						XBuff(Bits - 1);
--			y_out <=						YBuff(Bits - 1);
---------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------
--> The  CORDIC algorithm converges for angles between  -pi/2 to +pi/2.
--  The Quadrant Map subsystem always maps the absolute value for x-axis. 
--  This reflects the input vector from the 2nd and 3rd quadrant to the 1st and 4th quadrant
--> 2 Clk's Latency
---------------------------------------------------------------------------------------------
--> 1.Clk
			if NewDin = '1' then
				--> shift Input to Output Clk by Clk
				In2Out <=				In2Out(Bits - 2 downto 0) & '1';
				X(0) <=					x_in;--(Bits - 1 downto 1) & '1';	--> damit verhindern wir das x oder y NULL wird 
				Y(0) <=					y_in;--(Bits - 1 downto 1) & '1';	--> damit verhindern wir das x oder y NULL wird 
			else
				--> shift Input to Output Clk by Clk
				In2Out <=				In2Out(Bits - 2 downto 0) & '0';
			end if;
--> 2.Clk
			if X(0)(Bits - 1) = '0' then
				X(1) <=					0 + X(0);									--> +xbuf
			else
				X(1) <=					0 - X(0);									--> -xbuf
			end if;
			QM_X_sign <=				X(0)(Bits - 1);
			QM_Y_sign <=				Y(0)(Bits - 1);
			Y(1) <=						Y(0);
			Z(1) <=						(OTHERS => '0');							--> Phase Input is allways 0

			--> für QuadrantCorrect müssen wir uns merken was QuadrantMap getan hat
			--> wir schieben die entsprechenden Bits mit jeden Clk
			MR_X_sign <=				MR_X_sign(Bits - 4 downto 0) & QM_X_sign;
			MR_Y_sign <=				MR_Y_sign(Bits - 4 downto 0) & QM_Y_sign;

---------------------------------------------------------------------------------------------
--> The  CORDIC algorithm performs a vector ratation as a sequence of Micro Rotations.
--  The fine angle rotation operation is performed iteratively in stages (0..stages-1). 
--  The i-th PE rotates its input vector by an angle +/- atan(1/2^i),
--  driving its input y coordinate towards zero 
--> (y -> 0) ==> (x -> sqrt(x^2+y^2)*1.646760.
--  X = x(i) - y(i)/2 if y(i) < 0, otherwise X = x(i) + y(i)/2
--  Y = y(i) + x(i)/2 if y(i) < 0, otherwise Y = y(i) - x(i)/2
--  Z = z(i) - atan(1/2^i) if y(i) < 0, otherwise Z = z(i) + atan(1/2^i)
---------------------------------------------------------------------------------------------
			for N in 0 to (Bits - 3) loop
				if Y(N + 1)(Bits - 1) = '1' then		--> y(N) < 0
					X(N + 2) <=			X(N + 1) - Y(N + 1)((Bits - 1) downto N);
					Y(N + 2) <=			Y(N + 1) + X(N + 1)((Bits - 1) downto N);
					Z(N + 2) <=			Z(N + 1) - (AtanTable(N)(32 downto (32 - (Bits - 1))) + AtanTable(N)(32 - Bits));
				else										--> y(N) >= 0
					X(N + 2) <=			X(N + 1) + Y(N + 1)((Bits - 1) downto N);
					Y(N + 2) <=			Y(N + 1) - X(N + 1)((Bits - 1) downto N);
					Z(N + 2) <=			Z(N + 1) + (AtanTable(N)(32 downto (32 - (Bits - 1))) + AtanTable(N)(32 - Bits));
				end if;
			end loop;

---------------------------------------------------------------------------------------------
-- The CORDIC algorithm coverges for angles between -pi/2 to +pi/2.
-- The Quadrant Correct subsystem reflects the angle back to the 2nd and 3rd quadrant
-- from the 1st and 4th quadrant if reflection was applied during the quadarnt map stage.
-- Reflection is applied by subtracting the output angle by pi if the original vector was in the
-- 2nd quadrant and -pi if it was in the 3rd quadrant.
--> 1 Clk Latency
---------------------------------------------------------------------------------------------
--> 1.Clk
			if MR_X_sign(Bits - 3) = '0' then
				phase_out <=																											  Z(Bits - 1);
			else
				if MR_Y_sign(Bits - 3) = '0' then
					phase_out <=		(ConstPiPlus(32 downto (32 - (Bits - 1))) + ConstPiPlus(32 - Bits))		- Z(Bits - 1);
				else
					phase_out <=		(ConstPiMinus(32 downto (32 - (Bits - 1))) + ConstPiMinus(32 - Bits))	- Z(Bits - 1);
				end if;
			end if;
			mag_out <=																													  X(Bits - 1);
			--> put out delayed Input Values
			NewDout <=																													  In2Out(Bits - 1);

		end if; --elsif rising_edge(Clk) then
	end process CordicVectoringKernel;
end Behavioral;

