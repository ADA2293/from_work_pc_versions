function [] = XLink_RcvSim()

% first close all old figures
close all;

%#########################################################################
% define Data Types and Numbers for cordic_vectoring_kernel
%#########################################################################
sumWL_Vect =    12;                                             % CORDIC sum word length for vectoring (calc Phi and Pow) e.g. 12 for Q12.8
fract_Vect =    sumWL_Vect - 4;                                 % CORDIC fractional bit lenght for vectoring (calc Phi and Pow) e.g. 12 for Q12.8
NType_Vect =    numerictype(1, sumWL_Vect, fract_Vect);         % Data type for Binary Values for vectoring (calc Phi and Pow) e.g. 12 for Q12.8
niters_Vect =   fract_Vect + 2;                                 % Number of CORDIC iterations -> the same as fractional bit lenght +2
% Set fixed-point math settings for the defined NType 
F_Vect= fimath( 'SumMode','SpecifyPrecision',...
                'SumWordLength',sumWL_Vect,...
                'SumFractionLength',fract_Vect);

%#########################################################################
% define Data Types and Numbers for cordic_rotaton_kernel
%#########################################################################
sumWL_Rot =     16;                                             % CORDIC sum word length for rotation e.g. 12 for Q12.8
fract_Rot =     sumWL_Rot - 4;                                  % CORDIC fractional bit lenght for rotation
NType_Rot =     numerictype(1, sumWL_Rot, fract_Rot);           % Data type for Binary Values for rotation
niters_Rot =    fract_Rot + 2;                                  % Number of CORDIC iterations -> the same as fractional bit lenght +2
Factor_Rot =    2^fract_Rot;
% Set fixed-point math settings for the defined NType 
F_Rot = fimath( 'SumMode','SpecifyPrecision',...
                'SumWordLength',sumWL_Rot,...
                'SumFractionLength',fract_Rot);

%#########################################################################
% define NumSymb and Samp/Symb
%#########################################################################
NumSymb =       32;
NumSymb =       NumSymb + 10; % for resizes TestSamp, because FIR have to swing in first (150)
SampSymb =      16;


%#########################################################################
%define Noise and Phase parametres for TestSamp generation
%#########################################################################
dPhiSamp =      (pi / (500));                                   % define dphiSamp for Test data (TestSamp) max. 3/4*pi from sample to sample
Phase =          0;
%Phase =         (2 * pi * rand);                                % define a random start phase
%SNR =           10;                                             % define SNR(dB) for Test data (TestSamp) 6..40, if use awg()
noise =         0.001;                                          % define Noise
if sumWL_Rot == 12  RcvGain = 2500; end;                        % receive Gain -> eg. AGC control value for Q12.8
if sumWL_Rot == 16  RcvGain =  2500 * 16; end;                  % receive Gain -> eg. AGC control value for Q16.8



%#########################################################################
% create TestBits (one per Symbol) Bit values [+1/-1]
%#########################################################################
TestBits(NumSymb) = 0;
Bit =       1;
for i = 1: NumSymb,
    % create a random Bit value
%    if rand < .5  Bit = -1;  else   Bit = +1; end;

    % create always the same Bit value -> CW carrier
%    Bit =                               Bit; % copy BitValue
    
    % create an alternating Bit value -> acquisition data 0101010...
    Bit =                               -Bit; % toggle Bit value
    
    % build TestBits vector/array
    TestBits(i) =                       Bit;
end; % for i = 1: NumSymb,
%plot(TestBits, 'r');


%#########################################################################
% create TestSamp with SampSymb for NumSymb
%#########################################################################
for i = 1: NumSymb,
   TestSamp(SampSymb * (i - 1) + 1) =   TestBits(i);
end; % for i = 1: NumSymb,
%plot(TestSamp, 'r');


%#########################################################################
% upsampling TestSamp
%#########################################################################
h_upsamp = firrcos(150, .5, .2, 16); % upsampling Filter Coefficients
TestSamp = conv(TestSamp, h_upsamp);


%#########################################################################
% resizes TestSamp, because FIR have to swing in first
%#########################################################################
for i = 1: length(TestSamp) - 150,
    TestSamp(i) = TestSamp(i + 150);
end;
TestSamp = TestSamp(1:length(TestSamp) - 300);
%plot(TestSamp, 'r');


%#########################################################################
% add start Phase 45°
%#########################################################################
%TestSamp = TestSamp * exp(sqrt(-1) *  0.7854);


%#########################################################################
% add noise with a defined SNR(dB) to the normalized('measured') input data 
%#########################################################################
%TestSamp = awgn(TestSamp, SNR, 'measured');


%#########################################################################
%add noise and Phase for every TestSamp
%#########################################################################
noi = std(TestSamp) * noise; %normalize noise to Signal Power
for i = 1: length(TestSamp),
    Phase = Phase + dPhiSamp;
    if Phase > +pi  Phase = Phase - 2*pi; end;
    if Phase < -pi  Phase = Phase + 2*pi; end;
     
    TestSamp(i) = TestSamp(i) * exp(sqrt(-1) * Phase)  + 0.707 * noi * (randn + sqrt(-1) * randn);
end; % for i = 1: length(TestSamp),


%#########################################################################
% End TestSamp generation $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
%plot(TestSamp_real, TestSamp_imag, 'r');
%scatter(real(TestSamp), imag(TestSamp),  'r','.'); %plot only points
%#########################################################################


%#########################################################################
% -> for receive we handle the incoming samples in parts of 32 Symbols
% -> every symbol have 16 Samples a 12 Bit (+/- 4095)
% -> in XLink we have a receive AGC which should bring the Sample Values
%    to approx +/-110 -> 10*log(110^2 + 110^2) ~43.8dB Power
% -> we would interpret the 12 Bits as Q12.8 Format
% -> with the cordic_rotation_kernel we will derotate(correct)
%    the incoming samples with dPhi
% -> after this the cordic_vectoring_kernel will calculated Phi and Pow
%    for every sample in a symbol
%
% NOTE: -> in this simulation we have 16 samples per Symbol
%       -> in the real world we have f_Samp = n * 4 * f_RcvSymb
%#########################################################################

%#########################################################################
% normalize TestSamp -> approx +/-110 in Q.12.8 Format(/256) ~ -> (Rcv AGC)
%#########################################################################
TestSamp =                              TestSamp * (RcvGain) / Factor_Rot;
TestSamp_real =                         real(TestSamp);
TestSamp_imag =                         imag(TestSamp);


%#########################################################################
% Example calc Phi and Pow
%#########################################################################
I_In_Vect = fi(TestSamp_real, NType_Vect);                      % Fixed-point I_In values
Q_In_Vect = fi(TestSamp_imag, NType_Vect);                      % Fixed-point Q_In values

Pow_Vect =  fi(zeros(size(TestSamp_real)), NType_Vect);         % X out array pre-allocation
Y_Qut =     fi(zeros(size(TestSamp_real)), NType_Vect);         % Y out array pre-allocation -> should always converges to 0
Phi_Vect =  fi(zeros(size(TestSamp_imag)), NType_Vect);         % Z out array pre-allocation
Z_In =      fi(0, NType_Vect);                                  % Z input value could be always 0


figure('Name','Rotation Input IQ','Position', [1700, 480, 500, 500]);
scatter(I_In_Vect, Q_In_Vect,  'r','.'); %plot only points
axis([-1 1 -1 1]);
figure('Name','Rotation Input', 'Position', [2400, 480, 500, 500]);
stem(I_In_Vect, 'r'); hold on; stem(Q_In_Vect, 'b'); hold off;


for idx = 1:length(TestSamp_real)
    % CORDIC rotation kernel sample per sample
    [Pow_Vect(idx), Y_Qut(idx), Phi_Vect(idx)] = cordic_vectoring_kernel(I_In_Vect(idx), Q_In_Vect(idx), Z_In, niters_Vect, sumWL_Vect, fract_Vect);
end

figure('Name','Power', 'Position', [1700, -20, 500, 500]);
stem(Pow_Vect, 'r','.'); 
figure('Name','dPhi', 'Position', [2400, -20, 500, 500]);
stem(Phi_Vect, 'b');

%#########################################################################
% Example correct back dPhiSamp_Rot
%#########################################################################
% I_In = fi(TestSamp_real, NType_Rot);                            % Fixed-point I_In values
% Q_In = fi(TestSamp_imag, NType_Rot);                            % Fixed-point Q_In values
% 
% figure('Name','Rotation Input IQ','Position', [1700, 480, 500, 500]);
% scatter(I_In, Q_In,  'r','.'); %plot only points
% axis([-1 1 -1 1]);
% figure('Name','Rotation Input', 'Position', [2400, 480, 500, 500]);
% stem(I_In, 'r'); hold on; stem(Q_In, 'b'); hold off;
% 
% I_Rot = fi(zeros(size(TestSamp_real)), NType_Rot);              % X array pre-allocation
% Q_Rot = fi(zeros(size(TestSamp_imag)), NType_Rot);              % Y array pre-allocation
% Z_Rot = fi(zeros(size(TestSamp_real)), NType_Rot);              % Z array pre-allocation
% Pi_Rot = fi(pi,NType_Rot);
% Z_In = fi(pi, NType_Rot);                                       % Fixed-point dPhiSamp values (angle)
% % *1) !!!so geht das nicht
% %z_in = fi(pi, NType_Rot);
% dPhiSamp_Rot =  fi(dPhiSamp, NType_Rot);
% 
% for idx = 1:length(TestSamp_real)
%     % CORDIC rotation kernel sample per sample
%     [I_Rot(idx), Q_Rot(idx), Z_Rot(idx)] = cordic_rotation_kernel(I_In(idx), Q_In(idx), Z_In, niters_Rot, sumWL_Rot, fract_Rot);
%     Z_In = F_Rot.sub(Z_In ,dPhiSamp_Rot);
% % *1) !!!so geht das nicht
% %    z = z - dPhiSamp_Rot;
%     if Z_In > +Pi_Rot    Z_In = F_Rot.sub(Z_In, 2*Pi_Rot); end;
%     if Z_In < -Pi_Rot    Z_In = F_Rot.add(Z_In, 2*Pi_Rot); end;
% end
% figure('Name','Rotation Output IQ', 'Position', [1700, -20, 500, 500]);
% scatter(I_Rot, Q_Rot, 'r','.'); %plot only points
% axis([-1 1 -1 1]);
% figure('Name','Rotation Output', 'Position', [2400, -20, 500, 500]);
% stem(I_Rot, 'r'); hold on; stem(Q_Rot, 'b'); hold off;







