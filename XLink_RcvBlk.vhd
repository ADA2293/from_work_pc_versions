--/********************************************************************************************/
--/**
--! @defgroup XLink_RcvBlkDescription XLink_RcvBlk
--!
--! @brief		XLink_RcvBlk handles incomming ADC samples and caculates<br>
--!						decimatetd If-samples / Bb-samples or symbols for the ARM CPU for further processing.
--!
--! Target Devices:	Spartan 3ADSP 1800 <br>
--! Tool versions:	12.3 <br>
--!
--! @details	XLink_RcvBlk handles incomming ADC samples and caculates<br>
--!						decimatetd If-samples / Bb-samples or symbols for the ARM CPU for further processing.<br>
--!						We works always in Rx2Tx2 mode, this means two Rcv channels are always activated.<br>
--!						If one of the channels not used (RcvOn_x = 0), the incomming ADC samples will be ignored /skipped.<br>
--!						<b>a Register-bits and values:</b><br>
--!						--> Rcv control<br>
--!						Register 0x0040 ==> RcvCtr0<br>
--!							RcvCtr0( 3 downto  0)	-> RcvMode:			0x0-Transparent; 0x1-CalcPowPhi; 0x3-...<br>
--!							RcvCtr0( 5 downto  4)	-> RcvChSelect:	00-> RCV_OFF; 01-> RCV_CH0; 10-> RCV_CH1; 11-> RCV_BOTH<br>
--!							RcvCtr0(15 downto  6) -> Rsv0_a:			=> 10-Bit
--!							RcvCtr0(31 downto 16)	-> DropStep:		Step for Sample decimation (1...14.000)<br>
--!<br>
--!						Register 0x0044 ==> RcvState0<br>
--!<br>
--!						Register 0x0048 ==> RcvCtr1<br>
--!							RcvCtr1(11 downto  0)	-> dPhiCorrect:	phase correction value in Q12.8 Format<br>
--!							RcvCtr1(31 downto 12) -> Rsv1:			=> 20-Bit
--!<br>
--!						Register 0x004C ==> RcvState1<br>
--!<br>
--!						Register 0x0050 ==> RcvCtr2<br>
--!<br>
--!						Register 0x0054 ==> RcvState2<br>
--!
--! <b>a HEADLINE</b><br>
--! @image html a image for better description<br>
--!
--! @note				!!! notes for the vhdl modul.
--!
--/********************************************************************************************/

--/********************************************************************************************/
--! @file		XLink_RcvBlk.vhd
--! @copydoc	XLink_RcvBlkDescription
--!
--! Acronym:	XLink_RcvBlk <br>
--! Created on: 20.04.2020 <br>
--! <hr>
--! @copyright	(c) by IQ wireless, Berlin
--/********************************************************************************************/

--! use standard libary
library IEEE;
--! use logic elements
use IEEE.STD_LOGIC_1164.ALL;
--! use unsigned logic
use IEEE.STD_LOGIC_UNSIGNED.ALL;
--! use signed logic
--use IEEE.STD_LOGIC_SIGNED.ALL;

--! use unisim libary
library UNISIM;
--! use vcomponents
use UNISIM.VComponents.all;

--! @copydoc	XLink_RcvBlkDescription
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--<<<< hier Definieren wir die Hardware Signale des FPGA (wir sind auf dem Top Level)
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
entity XLink_RcvBlk is
--<<<< hier Generics festlegen (Project ???, Bitbreiten ...)
	GENERIC
		(
		SampBits:									natural := 12			--> für Variable Input Bitbreiten
		);
				
--<<<< hier die Namen der Ports festlegen
	PORT
		(
		i_Clk:										in	STD_LOGIC;																												--! Main Clock ~4ns before Rcv/XmtIn_Clk --> DIV by 4 = Rcv/XmtIn_Clk
		--> Modul Clocks
		AxiClk:										in	STD_LOGIC;																												--! the AXI Clock (100MHz)
		RcvClk :									in	STD_LOGIC;																												--! Modul Clock											-> Rcv Symbol Clock (RcvIn_Clk)  

		--> Modul Control(in) and State(out) Interface
		--> !!! AXI CLOCK DOMAIN !!!
		RcvCtr0 :									in	STD_LOGIC_VECTOR(31 downto 0)	:= (others => '0');									--! 0x0040	RcvCtr0			(wr by PS	-> rd by PL)
		RcvState0 :								out	STD_LOGIC_VECTOR(31 downto 0)	:= (others => '0');									--! 0x0044	RcvState0		(wr by PL	-> rd by PS)
		RcvCtr1 :									in	STD_LOGIC_VECTOR(31 downto 0)	:= (others => '0');									--! 0x0048	RcvCtr1			(wr by PS	-> rd by PL)
		RcvState1 :								out	STD_LOGIC_VECTOR(31 downto 0)	:= (others => '0');									--! 0x004C	RcvState1		(wr by PL	-> rd by PS)
		RcvCtr2 :									in	STD_LOGIC_VECTOR(31 downto 0)	:= (others => '0');									--! 0x0050	RcvCtr2			(wr by PS	-> rd by PL)
		RcvState2 :								out	STD_LOGIC_VECTOR(31 downto 0)	:= (others => '0');									--! 0x0054	RcvState2		(wr by PL	-> rd by PS)

		--> Flags for AXI Register write access
		PS_Wr2PL :								in	STD_LOGIC_VECTOR(31 downto 0);																		--! WrFlags for AXI Register(31 downto 0) (single pulse @ AxiClk) 

		--> Modul Data Input Interface
		RcvIn_Data :							in	STD_LOGIC_VECTOR(63 downto 0)	:= (others => '0');									--! data in for the Xlink_RcvBlk						(PACK->adc_data)
		RcvIn_WrEn :							in	STD_LOGIC											:= '0';															--! write enable; data for the Xlink_RcvBlk	(PACK->adc_valid)
		RcvIn_Sync :							in	STD_LOGIC											:= '0';															--! sync for the Xlink_RcvBlk								(PACK->adc_sync)
		RcvOut_OverFlow :					out	STD_LOGIC											:= '0';															--! overflow signal from the Xlink_RcvBlk 	(ADC_FIFO->dout_ovf)

		--> Modul Data Output Interface
		RcvOut_Data :							out	STD_LOGIC_VECTOR(63 downto 0)	:= (others => '0');									--! data for the ADC_DMA FiFo								(ADC_DMA->fifo_wr_din)
		RcvOut_WrEn :							out	STD_LOGIC											:= '0';															--! write enable to the ADC_DMA							(ADC_DMA->fifo_wr_en)
		RcvOut_Sync :							out	STD_LOGIC											:= '0';															--! sync for the ADC_DMA FiFo								(ADC_DMA->fifo_wr_sync)
		RcvIn_OverFlow :					in	STD_LOGIC											:= '0'															--! overflow signal from the ADC_DMA FiFo		(ADC_DMA->fifo_wr_overflow)

--> #########   DEBUG  ########### 
		;XLink_RcvBlk_DBG_Val :		out	STD_LOGIC_VECTOR(31 downto 0)	:= (others => '0')									--! 32 Bit Debug Value
		;XLink_RcvBlk_DBG:				out	STD_LOGIC_VECTOR (3 downto 0)	:= x"0"															--! debug signal[0..3]
--> ############################## 
			);
end XLink_RcvBlk;

--! @copydoc	XLink_RcvBlkDescription
architecture Behavioral of XLink_RcvBlk is
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--<<<< hier die shared Variablen oder Signale definieren
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--	--> Flags for AXI Register write access
--	signal PS_Wr2PL_In :				std_logic_vector(31 downto 0)											:= (others => '0');	--! WrFlags for AXI Register(31 downto 0) (AxiClk Domain) 

--	--> Clock Domain Crossing Buffers Axi2Rcv
--	signal PS_Wr2PL_B :					std_logic_vector(31 downto 0)											:= (others => '0');	--!	for Clock Domain Crossing Buffer stage 1 
--	signal PS_Wr2PL_BB :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 2
--	signal PS_Wr2PL_BBB :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 3

	signal RcvCtr0_B :					std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 1
	signal RcvCtr0_BB :					std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 2
	signal RcvCtr0_BBB :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 3
	signal RcvCtr1_B :					std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 1
	signal RcvCtr1_BB :					std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 2
	signal RcvCtr1_BBB :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 3
	signal RcvCtr2_B :					std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 1
	signal RcvCtr2_BB :					std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 2
	signal RcvCtr2_BBB :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 3

	--> Clock Domain Crossing Buffers Rcv2Axi
	signal RcvState0_RcvClk :		std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 0
	signal RcvState0_B :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 1
	signal RcvState0_BB :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 2
	signal RcvState1_RcvClk :		std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 0
	signal RcvState1_B :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 1
	signal RcvState1_BB :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 2
	signal RcvState2_RcvClk :		std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 0
	signal RcvState2_B :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 1
	signal RcvState2_BB :				std_logic_vector(31 downto 0)											:= (others => '0');	--! for Clock Domain Crossing Buffer stage 2

	--> Signals for Receive Control
	signal RcvMode:							std_logic_vector(3 downto 0)											:= (others => '0');	--! RcvMode: 0x0-Transparent; 0x1-CalcPowPhi; 0x2-...<br>
	signal RcvChSelect:					std_logic_vector(1 downto 0)											:= (others => '0');	--! receive channel select 00-> RCV_OFF; 01-> RCV_CH0; 10-> RCV_CH1; 11-> RCV_BOTH
	--> for Fsym > 4.000 ==> includes the case for Fsym > 64.000
	signal DropStep:						std_logic_vector(15 downto 0)											:= (others => '0');	--! number of steps over input Samples which dropped before the next BbS put out -> Decimationstep 1...14.000
	signal RcvSampPerSymb:			std_logic_vector(17 downto 0)											:= (others => '0');	--! 4...56.000 Samples per Symbol(min) = DropStep*4;
	
	--> Signals for RecDec (Rcv sample decimation)
--	signal RcvDecOutSymb:				std_logic_vector(63 downto 0)											:= (others => '0');	--! RecDec Output -> 16Bit I and 16Bit Q for both Rcv channels interleaved/alterneted 
--	signal RcvDecNextOutSymb:		std_logic																					:= '0';							--! RecDec -> the next Output Samples/Symbols are valid

	--> Signals for the assignment of the receiving channels and their splitting
	signal RcvSamp_Ch0_I:				std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! receive sample channel 0 Part I
	signal RcvSamp_Ch0_Q:				std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! receive sample channel 0 Part Q
	signal RcvSamp_Ch1_I:				std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! receive sample channel 1 Part I
	signal RcvSamp_Ch1_Q:				std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! receive sample channel 1 Part Q

	--> Signals for Receive Sample N and N+1
	signal RcvSamp_Curr_I:			std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! receive sample N+1 Part I
	signal RcvSamp_Curr_Q:			std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! receive sample N+1 Part Q

	--> Signals for Phase Correction 
	signal SumPhiCorr_Out:			std_logic																					:= '0';							--! a new SumPhiCorr valid
	signal SumPhiCorr:					std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! SumPhiCorr value
	signal CorrSamp_NewDout:		std_logic																					:= '0';							--! a new corrected sample is valid
	signal CorrSamp_I:					std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! corrected sample I
	signal CorrSamp_Q:					std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! corrected sample Q
	signal dPhiCorrect:					std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! phase correction value

	--> Signals for Phase and Magnitude calculations 
	signal MagPhi_NewDout:			std_logic																					:= '0';							--! new receive sample Magnitude and Phase valid
	signal RcvSamp_Mag:					std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! receive sample Magnitude NOTE: the magnitude scale factor is K = 1,646760
	signal RcvSamp_Phi:					std_logic_vector((SampBits - 1) downto 0)					:= (others => '0');	--! receive sample Phase
	

--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--<<<< Component Declaration --> hier geben wir das "Pinning" von SubComponnten an
--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--	---------------------------------------------------------------------------------------
--	--! @brief		CmpMult realized a complex multiplication operation.
--	---------------------------------------------------------------------------------------
--	COMPONENT CmpMult
--		GENERIC
--			(
--			Bits:										natural
--			);
--		PORT
--			(
--			Clk :										IN	std_logic;
--			Re_A :									IN	std_logic_vector((SampBits - 1) downto 0);
--			Im_A :									IN	std_logic_vector((SampBits - 1) downto 0);
--			Re_B :									IN	std_logic_vector((SampBits - 1) downto 0);
--			Im_B :									IN	std_logic_vector((SampBits - 1) downto 0);          
--			Re_Out :								OUT	std_logic_vector((SampBits + SampBits) downto 0);
--			Im_Out :								OUT std_logic_vector((SampBits + SampBits) downto 0)
--			);
--END COMPONENT;

--	---------------------------------------------------------------------------------------
--	--! @brief		RcvDec decimate the IF IQ-Samples.
--	--!						For this, passes only every NumDec samples from in to out.
--	---------------------------------------------------------------------------------------
--	COMPONENT RcvDec
--	PORT
--		(
--		SymbClk :									IN	std_logic;																												--! SymbClk for incomming Samples/Symbols
--		DropCnt :									IN	std_logic_vector(15 downto 0);																		--! number of input Samples/Symbols which dropped before the next put out (1...3.500)
--		InSymb :									IN	std_logic_vector(63 downto 0);																		--! Input:  16Bit I and 16Bit Q for both Rcv channels interleaved/alterneted
--		InWrEn :									IN	std_logic;																												--! WrEn:		next InSymb are valid
--		InSync :									IN	std_logic;																												--! InSync:	original fifo_wr_sync input (PACK->adc_sync) ??
--		OutSymb :									OUT	std_logic_vector(63 downto 0);																		--! Output: 16Bit I and 16Bit Q for both Rcv channels interleaved/alterneted 
--		NextOutSymb :							OUT	std_logic																													--! the next Output Samples/Symbols are valid
--		);
--	END COMPONENT;


	---------------------------------------------------------------------------------------
	--! @brief		This design calculates next RotPhi for a given PhiStep (dPhi)
	---------------------------------------------------------------------------------------
	COMPONENT CalcRotStep
		GENERIC
			(
			Bits:										natural
			);
		PORT
			(
			Rst :										IN	std_logic;
			Clk :										IN	std_logic;
			NewDin :								IN	std_logic;
			PhiStep :								IN	std_logic_vector((SampBits - 1) downto 0);          
			RotPhi :								OUT std_logic_vector((SampBits - 1) downto 0);
			NewDout :								OUT std_logic
			);
	END COMPONENT;


	---------------------------------------------------------------------------------------
	--! @brief	This design implements a fully parallel CORDIC (COordinate Rotation DIgital Computer) algorithm
	--!					in rotational mode that computes the sine and cosine of the input angle z between +/- pi.
	---------------------------------------------------------------------------------------
	COMPONENT VectRotate_Generic
		GENERIC
			(
			Bits:										natural
			);
		PORT
			(
			Rst :										IN	std_logic;
			Clk :										IN	std_logic;
			NewDin :								IN	std_logic;
			x_in :									IN	std_logic_vector((SampBits - 1) downto 0);
			y_in :									IN	std_logic_vector((SampBits - 1) downto 0);          
			z_in : 									IN	std_logic_vector((SampBits - 1) downto 0);          
			x_out : 								OUT std_logic_vector((SampBits - 1) downto 0);
			y_out : 								OUT std_logic_vector((SampBits - 1) downto 0);
			NewDout : 							OUT std_logic
			);
	END COMPONENT;


	---------------------------------------------------------------------------------------
	--! @brief		This design implements a rectangular-to-polar coordinate conversion
	--!						using a fully parallel CORDIC
	---------------------------------------------------------------------------------------
	COMPONENT Rect2Polar_Generic
		GENERIC
			(
			Bits:										natural
			);
		PORT
			(
			Rst :										IN	std_logic;
			Clk :										IN	std_logic;
			NewDin :								IN	std_logic;
			x_in :									IN	std_logic_vector((SampBits - 1) downto 0);
			y_in :									IN	std_logic_vector((SampBits - 1) downto 0);          
			mag_out :								OUT	std_logic_vector((SampBits - 1) downto 0);
			phase_out :							OUT	std_logic_vector((SampBits - 1) downto 0);
			NewDout :								OUT	std_logic
			);
	END COMPONENT;


begin
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--<<<< Component Instantiation	--> hier verbinden wir das "Pinning" von SubInstancen mit dieser Logic Ebene
											--> es ist möglich mehrere gleiche SubInstancen zu erzeugen (paralel in HW)
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--	---------------------------------------------------------------------------------------
--	--! @brief		CmpMult realized a complex multiplication operation.
--	---------------------------------------------------------------------------------------
--	Inst_CmpMult: CmpMult
--		GENERIC MAP
--			(
--			Bits =>									SampBits
--			)
--		PORT MAP
--			(
--			Clk => 									RcvClk,
--			Re_A =>									RcvSamp_Curr_I,
--			Im_A =>									RcvSamp_Curr_Q,
--			Re_B =>									RcvSamp_Last_I,
--			Im_B =>									RcvSamp_Last_Q,
--			Re_Out =>								RcvSamp_Mult_I,
--			Im_Out =>								RcvSamp_Mult_Q
--			);


--	---------------------------------------------------------------------------------------
--	--! @brief		RcvDec decimate the IF IQ-Samples.
--	--!						For this, passes only every NumDec samples from in to out.
--	---------------------------------------------------------------------------------------
--	Inst_RcvDec: RcvDec
--	PORT MAP
--		(
--		SymbClk => 								RcvClk,
--		DropCnt =>								DropStep,
--		InSymb =>									RcvIn_Data,
--		InWrEn =>									RcvIn_WrEn,
--		InSync =>									RcvIn_Sync,
--		OutSymb => 								RcvDecOutSymb,
--		NextOutSymb => 						RcvDecNextOutSymb
--		);


	---------------------------------------------------------------------------------------
	--! @brief		This design calculates next RotPhi for a given PhiStep (dPhi)
	---------------------------------------------------------------------------------------
	Inst_CalcRotStepBbS: CalcRotStep
	GENERIC MAP
		(
		Bits => 										SampBits
		)
	PORT MAP
		(
		Rst =>										'0',
		Clk =>										RcvClk,
		NewDin =>									'1', --> every RcvClk
		PhiStep =>								dPhiCorrect,
		RotPhi =>									SumPhiCorr,
		NewDout =>								SumPhiCorr_Out
		);


	---------------------------------------------------------------------------------------
	--! @brief	This design implements a fully parallel CORDIC (COordinate Rotation DIgital Computer) algorithm
	--!					in rotational mode that computes the sine and cosine of the input angle z between +/- pi.
	---------------------------------------------------------------------------------------
	Inst_VectRotate_Generic: VectRotate_Generic 
	GENERIC MAP
		(
		Bits => 									SampBits
		)
	PORT MAP
		(
		Rst =>										'0',
		Clk =>										RcvClk,
		NewDin =>									SumPhiCorr_Out,--'1',--> every RcvClk
		x_in =>										RcvSamp_Curr_I,
		y_in =>										RcvSamp_Curr_Q,
		z_in => 									SumPhiCorr,
		x_out =>									CorrSamp_I,
		y_out =>									CorrSamp_Q,
		NewDout =>								CorrSamp_NewDout
		);


	---------------------------------------------------------------------------------------
	--! @brief	This design implements a rectangular-to-polar coordinate conversion
	--!					using a fully parallel CORDIC
	---------------------------------------------------------------------------------------
	Inst_Rect2Polar_S16: Rect2Polar_Generic
	GENERIC MAP
		(
		Bits => 									SampBits
		)
	PORT MAP
		(
		Rst =>										'0',
		Clk =>										RcvClk,
		NewDin =>									CorrSamp_NewDout,--'1',--> every RcvClk
		x_in =>										CorrSamp_I,--RcvSamp_Curr_I,--
		y_in =>										CorrSamp_Q,--RcvSamp_Curr_Q,--
		mag_out =>								RcvSamp_Mag,
		phase_out =>							RcvSamp_Phi,
		NewDout =>								MagPhi_NewDout
		);


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--<<<< hier kommen unsere Ablaufsteuerungen hin 
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--	--------------------------------------------------------------------------------------------------
--	--! @brief		Buffer the WrFlags Pulse for AXI Register(31 downto 0) access,
--	--!						from the AXI Clock domain.
--	--!
--	--! @note			The Register Access Flag is only on AxiClk cycle activ.
--	--!
--	--! @param		AxiClk (input clock)
--	--------------------------------------------------------------------------------------------------
--	PS_Wr2PL_Proc : process (AxiClk)
--		begin
--			if rising_edge(AxiClk) then
--				--> loop for all 32 bits
--				for N in 0 to 31 loop
--					if PS_Wr2PL(N) = '1' then
--						PS_Wr2PL_In(N) <=						'1'; --> set the Wr2PL bit for the corresponding register 
--					end if; -- if PS_Wr2PL(N) = '1' then


---- FHA_ reset from PS_Wr2PL_In(N) via PS_Wr2PL_BBB(N) ???


--				end loop; -- for N in 0 to 31 loop
--			end if; -- if rising_edge(AxiClk) then
--	end process PS_Wr2PL_Proc;

		
	--------------------------------------------------------------------------------------------------
	--! @brief		Handles the Clock Domain Crossing of Signals
	--!						from the Rcv to AXI Clock domain
	--! @note			
	--! @param		AxiClk (input clock)
	--------------------------------------------------------------------------------------------------
	ClkDomCrossRcv2Axi : process (AxiClk)
		begin
			if rising_edge(AxiClk) then 
	--> 1. Clk/FF stage during Clock Domain Crossing (RcvClk -> AxiClk)
				RcvState0_B <=									RcvState0_RcvClk;
				RcvState1_B <=									RcvState1_RcvClk;
				RcvState2_B <=									RcvState2_RcvClk;

	--> 2. Clk/FF stage during Clock Domain Crossing (AxiClk -> AxiClk)
				RcvState0_BB <=									RcvState0_B;
				RcvState1_BB <=									RcvState1_B;
				RcvState2_BB <=									RcvState2_B;

	--> 3. Clk/FF stage during Clock Domain Crossing (AxiClk -> AxiClk)
				RcvState0 <=										RcvState0_BB;
				RcvState1 <=										RcvState1_BB;
				RcvState2 <=										RcvState2_BB;

			end if; -- if rising_edge(AxiClk) then
	end process ClkDomCrossRcv2Axi;


	-----------------------------------ps---------------------------------------------------------------
	--! @brief		Handles the Clock Domain Crossing of Signals
	--!						from the AXI to Rcv Clock domain
	--! @note			
	--! @param		RcvClk (input clock)
	--------------------------------------------------------------------------------------------------
	ClkDomCrossAxi2Rcv : process (RcvClk)
		begin
			if rising_edge(RcvClk) then 
--> 1. Clk/FF stage during Clock Domain Crossing (AxiClk -> RcvClk)
--			PS_Wr2PL_B <=										PS_Wr2PL_In;
				RcvCtr0_B <=										RcvCtr0;
				RcvCtr1_B <=										RcvCtr1;
				RcvCtr2_B <=										RcvCtr2;
				
--> 2. Clk/FF stage during Clock Domain Crossing (RcvClk -> RcvClk)
--				PS_Wr2PL_BB <=									PS_Wr2PL_B;
				RcvCtr0_BB <=										RcvCtr0_B;
				RcvCtr1_BB <=										RcvCtr1_B;
				RcvCtr2_BB <=										RcvCtr2_B;
		
--> 3. Clk/FF stage during Clock Domain Crossing (RcvClk -> RcvClk)
--				PS_Wr2PL_BBB <=									PS_Wr2PL_BB;
				RcvCtr0_BBB <=									RcvCtr0_BB;
				RcvCtr1_BBB <=									RcvCtr1_BB;
				RcvCtr2_BBB <=									RcvCtr2_BB;

--> 4.Clk	--> handle incomming data from AXI Register write access for ClkDomain RcvClk(SymbClk)
				-->  0-Version(rd);  1-IrqMask(wr);  2-IrqFlags(rd);  3-IrqClear(wr)
				--> 16-RcvCtr0(wr); 17-RcvState0(rd); 18-RcvCtr1(wr); 19-RcvState1(rd); 20-RcvCtr2(wr); 21-RcvState2(rd);
				-------------------------------------------------------------------------------------------
				--> 16	RcvCtr0 (0x0040)
--				if PS_Wr2PL_BBB(16) = '1' then
					RcvMode <=										RcvCtr0_BBB( 3 downto  0);																	--> RcvCtr0( 3 downto  0) -> RcvMode:			0x0-Transparent; 0x1-CalcPowPhi; 0x2-...
					RcvChSelect <=								RcvCtr0_BBB( 5 downto  4);																	--> RcvCtr0( 5 downto  4)	-> RcvChSelect:	00-> RCV_OFF; 01-> RCV_CH0; 10-> RCV_CH1; 11-> RCV_BOTH
--				RcvCtr0(15 downto 6) -> Rsv0_a:			=> 10-Bit
					DropStep <=										RcvCtr0_BBB(31 downto 16);																	--> RcvCtr0(31 downto 16)	-> DropStep:		Step for Sample decimation (1...14.000)
					RcvSampPerSymb <=							RcvCtr0_BBB(31 downto 16) & "00";														--> 4...56.000 Samples per Symbol = DropStep*4;
-->!!!					PS_Wr2PL_BBB(16) <= 					'0';																												--> Register write access has been processed now
--				end if; -- if PS_Wr2PL_BBB(16) = '1' then

				--> 18	RcvCtr1 (0x0048)
--				if PS_Wr2PL_BBB(18) = '1' then
					dPhiCorrect <=								RcvCtr1_BBB(11 downto  0);																	-->	RcvCtr1(11 downto  0)	-> dPhiCorrect:	phase correction value in Q12.8 Format<br>
--				RcvCtr1(31 downto 12) -> Rsv1:			=> 20-Bit
-->!!!					PS_Wr2PL_BBB(18) <= 					'0';																												--> Register write access has been processed now
--				end if; -- if PS_Wr2PL_BBB(18) = '1' then
					
				--> 20	RcvCtr2 (0x0050)
--				if PS_Wr2PL_BBB(20) = '1' then
-->!!!					PS_Wr2PL_BBB(20) <= 					'0';																												--> Register write access has been processed now
--				end if; -- if PS_Wr2PL_BBB(20) = '1' then
	
	
-- FHA_ reset from PS_Wr2PL_In(N) via PS_Wr2PL_BBB(N) ???
	
	
				-->	collect here all informations from the RcvClk Domain for RcvStateX,
				--> which should be sent via AXI to the ARM
				--	e.g. --------------------------------------
				RcvState0_RcvClk(3 downto 0) <= RcvMode;
				RcvState0_RcvClk(5 downto 4) <= RcvChSelect;
				
				RcvState1_RcvClk(11 downto 0) <= dPhiCorrect;
				-----------------------------------------------


			
		end if; -- if rising_edge(RcvClk) then 
	end process ClkDomCrossAxi2Rcv;


	--------------------------------------------------------------------------------------------------
	--! @brief		Handles the RcvSamples
	--!						-> buffer the incoming receive sample data and split into channels I/Q parameters
	--! @note			
	--! @param		RcvClk (input clock)
	--------------------------------------------------------------------------------------------------
	RcvSampleSave : process (RcvClk)
		begin
			if rising_edge(RcvClk) then 
				if RcvIn_WrEn = '1' then
					--> buffer the incoming receive sample data and split into channels I/Q parameters
					RcvSamp_Ch0_I <=								RcvIn_Data(11 downto  0);																		--> the 16LSB's are I-Ch-0; but only 12 Bits are relevant
					RcvSamp_Ch0_Q <=								RcvIn_Data(27 downto 16);																		--> the next 16 are Q-Ch-0; but only 12 Bits are relevant
					RcvSamp_Ch1_I <=								RcvIn_Data(43 downto 32);																		--> the next 16 are I-Ch-1; but only 12 Bits are relevant
					RcvSamp_Ch1_Q <=								RcvIn_Data(59 downto 48);																		--> the 16MSB's are Q-Ch-1; but only 12 Bits are relevant
				end if; -- if RcvIn_WrEn = '1' then
			end if; -- if rising_edge(RcvClk) then 
		end process RcvSampleSave;


	--------------------------------------------------------------------------------------------------
	--! @brief		Handles the RcvSamples
	--!						-> select channel data for further processing
	--! @note			
	--! @param		RcvClk (input clock)
	--------------------------------------------------------------------------------------------------
	RcvSampleSelect : process (RcvClk)
		begin
			if rising_edge(RcvClk) then 
				--> for the current sample values select channel data for further processing 
				if		RcvChSelect = "01" then 
					--> RCV_CH0
					RcvSamp_Curr_I <=							RcvSamp_Ch0_I;
					RcvSamp_Curr_Q <=							RcvSamp_Ch0_Q;

				elsif		RcvChSelect = "10" then
					--> RCV_CH1
					RcvSamp_Curr_I <=							RcvSamp_Ch1_I;
					RcvSamp_Curr_Q <=							RcvSamp_Ch1_Q;
				
--				elsif		RcvChSelect = "11" then
--					--> RCV_BOTH ->  !!!!- NOT SUPPORTED -!!!!
--					RcvSamp_Curr_I <=							(others => '0');
--					RcvSamp_Curr_Q <=							(others => '0');
					
				else
					--> RCV_OFF and NOTSUPPORTED
					RcvSamp_Curr_I <=							x"800";																											--> use as marker for fail RcvCh selection
					RcvSamp_Curr_Q <=							x"7FF";																											--> use as marker for fail RcvCh selection
				end if;
			end if; -- if rising_edge(RcvClk) then 
		end process RcvSampleSelect;


	--------------------------------------------------------------------------------------------------
	--! @brief		Handles the multiplexing for data transfer via AXI to ARM-CPU
	--! @note			
	--! @param		RcvClk (input clock)
	--------------------------------------------------------------------------------------------------
	RcvData2ArmSelect : process (RcvClk)
		begin
			if rising_edge(RcvClk) then 
				
				--> WAS MACHEN WIR HIERMIT ?????
				RcvOut_OverFlow <=												RcvIn_OverFlow;
				RcvOut_Sync <=														RcvIn_Sync;
				RcvOut_WrEn <=														'0';																							--> set first to 0

				case RcvMode is
					--> 0x0-Transparent
					when "0000" =>
						RcvOut_WrEn <=												'1';																							--> put out 64 Bit with every RcvClk
						RcvOut_Data <=												RcvIn_Data;																				--> 64 Bit (2*32) original RcvIn_Data
----------------------------------------------------------------------------------------------------

				--> 0x1-CalcPowPhi + original Samples
					when "0001" =>
						RcvOut_WrEn <=												'1';																							--> put out 64 Bit every RcvClk
						RcvOut_Data(15 downto 0) <=						RcvSamp_Curr_I(11) & RcvSamp_Curr_I(11) & RcvSamp_Curr_I(11) & RcvSamp_Curr_I(11) & RcvSamp_Curr_I;	--> 16Bit I
						RcvOut_Data(31 downto 16) <=					RcvSamp_Curr_Q(11) & RcvSamp_Curr_Q(11) & RcvSamp_Curr_Q(11) & RcvSamp_Curr_Q(11) & RcvSamp_Curr_Q;	--> 16Bit Q
						--> NOTE Phi and Mag have a delay of 12 RcvClk's
						RcvOut_Data(43 downto 32) <=					RcvSamp_Phi;																			--> 12Bit Phi in Q4.8 Format 
						RcvOut_Data(47 downto 44) <=					RcvSamp_Phi(11) & RcvSamp_Phi(11) & RcvSamp_Phi(11) & RcvSamp_Phi(11); --> sign extention for RcvSamp_Phi to 16 Bit 
						RcvOut_Data(59 downto 48) <=					RcvSamp_Mag;																			--> 12Bit Magnitude; NOTE: the magnitude scale factor is K = 1,646760
						RcvOut_Data(63 downto 60) <=					"0000";																						--> sign extention for RcvSamp_Mag to 16 Bit -> should be always positiv 
----------------------------------------------------------------------------------------------------
					

				--> 0x2-CalcPowPhi + corrected Samples
					when "0010" =>
						RcvOut_WrEn <=												'1';																							--> put out 64 Bit every RcvClk
						RcvOut_Data(15 downto 0) <=						CorrSamp_I(11) & CorrSamp_I(11) & CorrSamp_I(11) & CorrSamp_I(11) & CorrSamp_I;	--> 16Bit I
						RcvOut_Data(31 downto 16) <=					CorrSamp_Q(11) & CorrSamp_Q(11) & CorrSamp_Q(11) & CorrSamp_Q(11) & CorrSamp_Q;	--> 16Bit Q
						--> NOTE Phi and Mag have a delay of 12 RcvClk's
						RcvOut_Data(43 downto 32) <=					RcvSamp_Phi;																			--> 12Bit Phi in Q4.8 Format 
						RcvOut_Data(47 downto 44) <=					RcvSamp_Phi(11) & RcvSamp_Phi(11) & RcvSamp_Phi(11) & RcvSamp_Phi(11); --> sign extention for RcvSamp_Phi to 16 Bit 
						RcvOut_Data(59 downto 48) <=					RcvSamp_Mag;																			--> 12Bit Magnitude; NOTE: the magnitude scale factor is K = 1,646760
						RcvOut_Data(63 downto 60) <=					"0000";																						--> sign extention for RcvSamp_Mag to 16 Bit -> should be always positiv 
----------------------------------------------------------------------------------------------------

					--> all others
					when others =>
						RcvOut_WrEn <=												'1';
						RcvOut_Data <=												x"CafeAffe" & x"DeadBeef";												--> use as marker for invalid RcvMode
----------------------------------------------------------------------------------------------------
				
				end case; -- case RcvMode is
					
			end if; -- if rising_edge(RcvClk) then 
	end process RcvData2ArmSelect;



	--------------------------------------------------------------------------------------------------
	--! @brief		Handle Rcv LoopBack 
	--! @note			for Test witout RcvBlk Data Handling
	--! @param		XmtIn_Clk (input clock)
	--------------------------------------------------------------------------------------------------
--	RcvOut_Clk <=								'0';
--	RcvOut_OverFlow <=					RcvIn_OverFlow;
--	RcvOut_Data <=							RcvIn_Data;
--	RcvOut_Sync <=							RcvIn_Sync;
--	RcvOut_WrEn <=							RcvIn_WrEn;


end Behavioral;
--/*************	eof: XLink_RcvBlk.vhd														***********************/
