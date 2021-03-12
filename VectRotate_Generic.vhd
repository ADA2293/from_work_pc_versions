----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    13:38:02 06/20/2011 
-- Design Name: 
-- Module Name:    VectRotate_S16 - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
-- This design implements a fully parallel CORDIC (COordinate Rotation DIgital Computer) algorithm
-- in rotational mode that computes the sine and cosine of the input angle z between +/- pi.
-- That is, given a complex-input <x,y>, and phase <z> it computes a new vector <X',Y'>, <Z' = 0> where 
-- X' = K(x*cos(z) - y*sin(z))
-- Y' = K(y*cos(z) + x*sin(z))
-- Z' -> 0
-- !! As is common, the magnitude scale factor K = 1,646760... (for 8 stages) is not compensated in the processor!!
--
-- The CORDIC algorithm is implemented in 3 steps.
-- Step 1: Coarse Angle Rotation. The algorithm converges only for angles between -pi/2 and +pi/2, so if z < -pi/2 or z > +pi/2, z transformed in the valid range. 
-- Step 2: Fine Angle Rotation.  For vector rotation, the resulting vector is rotated through progressively smaller angles, such that z goes
--         to zero. In the i-th stage, the angular rotation is by either +/- atan(1/2^i), depending on whether or not its input z is less than or greater than zero. 
-- Step 3: Angle Correction. If there was a transformation applied in Step 1, this step applies the appropriate vector correction.  
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
--
-- Additional Comments: 
-- wir verwenden als Zahlenformat z.B. Fix_16_12 -> Bit[15] Vorzeichen; Bit[14..12] Ganzahl; Bit[11..0] Fractional
--> x_in auf +/-3.4 begrenzen
--> y_in auf +/-3.4 begrenzen
--> z_in auf +/-Pi  begrenzen
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;

entity VectRotate_Generic is
	generic
			(
			Bits:						natural := 16			--> für Variable Input Bitbreiten
			);

    Port ( Rst :					in		STD_LOGIC;
           Clk :					in		STD_LOGIC;
			  NewDin :				in		STD_LOGIC;
           x_in :					in		STD_LOGIC_VECTOR (Bits - 1 downto 0);							--> auf +/-3.4 begrenzen
           y_in :					in		STD_LOGIC_VECTOR (Bits - 1 downto 0);							--> auf +/-3.4 begrenzen
           z_in :					in		STD_LOGIC_VECTOR (Bits - 1 downto 0);							--> auf +/-Pi  begrenzen
           x_out :				out	STD_LOGIC_VECTOR (Bits - 1 downto 0);
           y_out :				out	STD_LOGIC_VECTOR (Bits - 1 downto 0);
  			  NewDout:				out	STD_LOGIC
			  );
			  
end VectRotate_Generic;

architecture Behavioral of VectRotate_Generic is
	--> Signale für QuadrantMap
	signal	QM_AngMap:					STD_LOGIC										:= '0';
	signal	QM_Z_sign:					STD_LOGIC										:= '0';
	signal	MR_AngMap:					STD_LOGIC_VECTOR (Bits - 3 downto 0)	:=(OTHERS => '0');
	signal	MR_Z_sign:					STD_LOGIC_VECTOR (Bits - 3 downto 0)	:=(OTHERS => '0');
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
--	constant AtanTable:					AtanTableType									:=	(	x"0C90FDAA", x"076B19C1", x"03EB6EBF",	x"01FD5BAA",
--																											x"00FFAADE", x"007FF557", x"003FFEAB", x"001FFFD5", 
--																											x"000FFFFB", x"0007FFFF", x"00040000",	x"00020000",
--																											x"00010000", x"00008000", x"00004000", x"00002000",
--																											x"00001000", x"00000800", x"00000400", x"00000200",
--																											x"00000100", x"00000080", x"00000040", x"00000020",
--																											x"00000010", x"00000008", x"00000004", x"00000002",
--																											x"00000001", x"00000000");
--	constant	ConstHalfPiPlus:			STD_LOGIC_VECTOR (31 downto 0)	:= 			x"1921FB54";
--	constant	ConstHalfPiMinus:			STD_LOGIC_VECTOR (31 downto 0)	:= 			x"E6DE04AC";
--> ein Bit mehr damit wir auch bei Bits = 32 noch runden können
type AtanTableType is array( integer range 0 to 29) of	STD_LOGIC_VECTOR(32 downto 0);
	constant AtanTable:					AtanTableType									:=	(	'0' & x"1921FB54", '0' & x"0ED63383", '0' & x"07D6DD7E", '0' & x"03FAB753",
																											'0' & x"01FF55BB", '0' & x"00FFEAAE", '0' & x"007FFD55", '0' & x"003FFFAB",
																											'0' & x"001FFFF5", '0' & x"000FFFFF", '0' & x"00080000", '0' & x"00040000",
																											'0' & x"00020000", '0' & x"00010000", '0' & x"00008000", '0' & x"00004000",
																											'0' & x"00002000", '0' & x"00001000", '0' & x"00000800", '0' & x"00000400",
																											'0' & x"00000200", '0' & x"00000100", '0' & x"00000080", '0' & x"00000040",
																											'0' & x"00000020", '0' & x"00000010", '0' & x"00000008", '0' & x"00000004",
																											'0' & x"00000002", '0' & x"00000001");
	constant	ConstHalfPiPlus:			STD_LOGIC_VECTOR (32 downto 0)			:= 	'0' & x"3243F6A9";
	constant	ConstHalfPiMinus:			STD_LOGIC_VECTOR (32 downto 0)			:= 	'1' & x"CDBC0957";
	constant	ConstPiPlus:				STD_LOGIC_VECTOR (32 downto 0)			:=		'0' & x"6487ED51";
	constant	ConstPiMinus:				STD_LOGIC_VECTOR (32 downto 0)			:=		'1' & x"9B7812AF";
	constant	Const2PiPlus:				STD_LOGIC_VECTOR (32 downto 0)			:=		'0' & x"C90FDAA2";

---------------------------------------------------------------------------------'-----------------------------------

begin
---------------------------------------------------------------------------------------------
--> !!!!!!! wg. loop muß alles in einen Process !!!!!!
--> QuadrantMap + MicroRotation + QuadrantCorrect
---------------------------------------------------------------------------------------------
CordicRotationKernel: process(Clk)
	begin
		if rising_edge(Clk) then
---------------------------------------------------------------------------------------------
--> The  CORDIC algorithm converges for angles between  -pi/2 to +pi/2.
--  The Quadrant Map subsystem always maps z in the valid range. 
--> QuadrantMap; --> 2 Clk's Latency
---------------------------------------------------------------------------------------------
--> 1.Clk ==> buffer input values
			if NewDin = '1' then
				X(0) <=					x_in;
				Y(0) <=					y_in;
				Z(0) <=					z_in;
--> Erweiterung z_in auf +/-2Pi---------------------------------------------------------------------------------------------------------------------------------------------------------- 				
				if		z_in > (ConstPiPlus(32 downto (32 - (Bits - 1))) + ConstPiPlus(32 - Bits)) then
					Z(0) <=				z_in - (Const2PiPlus(32 downto (32 - (Bits - 1))) + Const2PiPlus(32 - Bits));
				elsif z_in < (ConstPiMinus(32 downto (32 - (Bits - 1)))  + ConstPiMinus(32 - Bits)) then
					Z(0) <=				z_in + (Const2PiPlus(32 downto (32 - (Bits - 1))) + Const2PiPlus(32 - Bits));
				end if;
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
				--> shift Input to Output Clk by Clk
				In2Out <=				In2Out(Bits - 2 downto 0) & '1';
			else
				--> shift Input to Output Clk by Clk
				In2Out <=				In2Out(Bits - 2 downto 0) & '0';
			end if;

--> 2.Clk
			X(1) <=						X(0);
			Y(1) <=						Y(0);
			if 	Z(0) > (ConstHalfPiPlus(32 downto (32 - (Bits - 1))) + ConstHalfPiPlus(32 - Bits)) then
				--> z > +pi/2 ==> sub +pi/2
				Z(1) <=					Z(0) - (ConstHalfPiPlus(32 downto (32 - (Bits - 1)))  + ConstHalfPiPlus(32 - Bits));
				QM_AngMap <=			'1'; 
				QM_Z_sign <= 			'0';

			elsif Z(0) < (ConstHalfPiMinus(32 downto (32 - (Bits - 1)))  + ConstHalfPiMinus(32 - Bits)) then
				--> z < -pi/2 ==> add +pi/2
				Z(1) <=					Z(0) + (ConstHalfPiPlus(32 downto (32 - (Bits - 1)))  + ConstHalfPiPlus(32 - Bits));
				QM_AngMap <=			'1'; 
				QM_Z_sign <= 			'1';

			else
				--> z > -pi/2  and z < +pi/2		==>	inside valid range
				Z(1) <=					Z(0);
				QM_AngMap <=			'0';
				QM_Z_sign <=			Z(0)(Bits - 1);
			end if;

			--> für QuadrantCorrect müssen wir uns merken was QuadrantMap getan hat
			--> wir schieben die entsprechenden Bits mit jeden Clk
			MR_AngMap <=				MR_AngMap(Bits - 4 downto 0) & QM_AngMap;
			MR_Z_sign <=				MR_Z_sign(Bits - 4 downto 0) & QM_Z_sign;

--------------------------------------------------------------------------------------------
--> The  CORDIC algorithm performs a vector ratation as a sequence of Micro Rotations.
--  The fine angle rotation operation is performed iteratively in stages (0..stages-1). 
--  The i-th PE rotates its input vector by an angle +/- atan(1/2^i),
--  driving its input z coordinate towards zero 
--> (z -> 0) ==> X' = K(x*cos(z) - y*sin(z)); Y' = K(y*cos(z) + x*sin(z))
--> K = 1,646760... (for 8 stages)
--  X = x(N) + y(N)/2 if z(i) < 0, otherwise X = x(N) - y(N)/2
--  Y = y(N) - x(N)/2 if z(i) < 0, otherwise Y = y(N) + x(N)/2
--  Z = z(N) + atan(1/2^N) if y(i) < 0, otherwise Z = z(N) - atan(1/2^N)
--> MicroRotation; --> (Bits - 3) Clk's Latency
---------------------------------------------------------------------------------------------
--> 3.Clk...15.Clk
			for N in 0 to (Bits - 3) loop
				if Z(N + 1)(Bits - 1) = '1' then		--> z(N) < 0
					X(N + 2) <=			X(N + 1) + Y(N + 1)((Bits - 1) downto N);
					Y(N + 2) <=			Y(N + 1) - X(N + 1)((Bits - 1) downto N);
					Z(N + 2) <=			Z(N + 1) + (AtanTable(N)(32 downto (32 - (Bits - 1))) + AtanTable(N)(32 - Bits));
				else										--> z(N) >= 0
					X(N + 2) <=			X(N + 1) - Y(N + 1)((Bits - 1) downto N);
					Y(N + 2) <=			Y(N + 1) + X(N + 1)((Bits - 1) downto N);
					Z(N + 2) <=			Z(N + 1) - (AtanTable(N)(32 downto (32 - (Bits - 1))) + AtanTable(N)(32 - Bits));
				end if;
			end loop;

---------------------------------------------------------------------------------------------
-- The CORDIC algorithm coverges for angles between -pi/2 to +pi/2.
-- The Quadrant Correct subsystem reflects the vector back to the right quadrant.
--> QuadrantCorrect; --> 1 Clk's Latency
---------------------------------------------------------------------------------------------
--> 16.Clk
			if MR_AngMap(Bits - 3) = '0' then
				x_out <=					X(Bits - 1);
				y_out <=					Y(Bits - 1);
			else
				if MR_Z_sign(Bits - 3) = '0' then
					x_out <=				0 - Y(Bits - 1);
					y_out <=				0 + X(Bits - 1);

				else
					x_out <=				0 + Y(Bits - 1);
					y_out <=				0 - X(Bits - 1);
				end if;
			end if;

			NewDout <= In2Out(Bits - 1);
--NewDout <= X(Bits - 1)(15) XOR Y(Bits - 1)(15);

		end if; --elsif rising_edge(Clk) then
	end process CordicRotationKernel;

end Behavioral;

