function[x_data,y_data,frequenz]=xlink_demodulator(noise,delta_f)

h=firrcos(150,.5,.2,16);%Tx Filter-Koeffizienten
r=-1;
    for i=1:260000,
        r=1;if rand<.5 r=-1;end;%random data
        %r=r;%CW carrier, unmoduliert
        %r=-r;%acquisition data 0101010...
        a(16*(i-1)+1)=r;if i==14991 a(16*(i-1)+1)=1;end;%Tx_Daten mit 16 Samplewerten pro Symbol
        x_data(i)=r;if i==14989 x_data(i)=1;end;if i==14990 x_data(i)=1;end;if i==14991 x_data(i)=1;end;
        %Eingangsdaten
    end;

b=conv(a,h);%Filterung der Tx-Daten
noi=std(b)*noise;%Normierung des Rauschens auf die Signalleistung
korr_messwert=0;
phi=2*pi*rand;%zufällige Anfangsphase des Tx-Signals
flag=0;
for i=1:length(b),%Frequenzversatz und Rauschen werden zum Tx-Signal hinzugefügt
    if delta_f>0.15 flag=1;end%obere Grenze für die Frequenzabweichung
    if delta_f<-0.15 flag=0;end%untere Grenze für die Frequenzabweichung
    if flag==0 delta_f=delta_f+0.00000001;end%lineare Frequenzänderung bis zur maximal positiven Ablage
    if flag==1 delta_f=delta_f-0.00000001;end%lineare Frequenzänderung bis zur maximal negativen Ablage
    
    b1(i)=b(i)*exp(sqrt(-1)*(delta_f*i+phi))+0.7*noi*(randn+sqrt(-1)*randn);
    bi(i)=real(b1(i));%Bildung von Inphase-Signal
    bq(i)=imag(b1(i));%Bildung von Quadratur-Signal
end

phase=0;%laufende Momentanphase 
frequenz(1)=0;%Anfangswert für Frequenzschätzung
dt=16*round(rand);%zufälliger Zeitversatz des ersten Signalsample-Wertes

for j=1:2000,%Anzahl der prozessierten Datenblöcke mit jeweils 128 Symbolen
    for k=1:128,%Frequenz und Timingschätzungen werden jeweils in Blöcken von 128 Symbolen durchgeführt
        i=128*(j+4)+k;%der erste Block beginnt nicht bei 0, weil das Tx-Filter erst einschwingen muss
       % t1(k)=-(b1(16*(i-1)+16+dt)-b1(16*(i-1)+dt))*b1(16*(i-1)+8+dt)/(0.025*(abs(b1(16*(i-1)+16+dt))+abs(b1(16*(i-1)+dt)))*(abs(b1(16*(i-1)+16+dt))+abs(b1(16*(i-1)+dt))));
        koeff1=abs(bi(16*(i-1)+16+dt))+abs(bi(16*(i-1)+dt));%Ermittlung der I-Amplituden für 2 Samplewerte, die je 1 Symbol Abstand haben 
        koeff2=abs(bq(16*(i-1)+16+dt))+abs(bq(16*(i-1)+dt));%Ermittlung der I-Amplituden für 2 Samplewerte, die je 1 Symbol Abstand haben
        koeffi=koeff1/(koeff1+koeff2);%Wichtung, damit größere Amplitude mehr Einfluss hat
        koeffq=koeff2/(koeff1+koeff2);%Wichtung, damit größere Amplitude mehr Einfluss hat
        ti=-(bi(16*(i-1)+16+dt)-bi(16*(i-1)+dt))*bi(16*(i-1)+8+dt)/(0.025*(abs(bi(16*(i-1)+16+dt))+abs(bi(16*(i-1)+dt)))*(abs(bi(16*(i-1)+16+dt))+abs(bi(16*(i-1)+dt))));
        tq=-(bq(16*(i-1)+16+dt)-bq(16*(i-1)+dt))*bq(16*(i-1)+8+dt)/(0.025*(abs(bq(16*(i-1)+16+dt))+abs(bq(16*(i-1)+dt)))*(abs(bq(16*(i-1)+16+dt))+abs(bq(16*(i-1)+dt))));
        %Gardner-Algorithmus zur Ermittlung des tatsächlichen
        %Zeit-Versatzes zur Symbolmitte. Normierung auf 1/16 Symboldauer
        t1(k)=koeffi*ti+koeffq*tq;
      
        y(i)=b1(16*(i-1)+dt);%das ist der komplexe Samplewert entsprechend des momentan ermittelten Zeitversatzes
        %jetzt werden die 4 Basebandwerte pro Symbol anhand der ermittelten Frequenz ins Basisband transformiert
        %da ein Baseband-Sample die Dauer von 4 Samplewerte hat, muss die Phase jeweils um 4 Frequenzsteps verändert werden  
        phase=phase+4*frequenz(j);
        ybb(4*(i-1)+1)=b1(16*(i-1)+dt)*exp(-sqrt(-1)*phase);%4*frequenz(j)*(4*(i-1)+1));
        phase=phase+4*frequenz(j);
        ybb(4*(i-1)+2)=b1(16*(i-1)+4+dt)*exp(-sqrt(-1)*phase);%4*frequenz(j)*(4*(i-1)+2));
        phase=phase+4*frequenz(j);
        ybb(4*(i-1)+3)=b1(16*(i-1)+8+dt)*exp(-sqrt(-1)*phase);%4*frequenz(j)*(4*(i-1)+3));
        phase=phase+4*frequenz(j);
        ybb(4*(i-1)+4)=b1(16*(i-1)+12+dt)*exp(-sqrt(-1)*phase);%4*frequenz(j)*(4*(i-1)+4)); 
        %ybb(i)=y(i)*(exp(-sqrt(-1)*16*frequenz(j)*i));
        
        df(k)=0.5*(angle(b1(16*(i-1)+dt)/b1(16*(i-1)+dt-1))+angle(b1(16*(i-1)+dt+1)/b1(16*(i-1)+dt)));%df ist die gemittelte Phasendifferenz (=Frequenz) für 2 aufeinanderfolgende Samplewerte um die Symbolmitte
        
    end;
      
    messwert=(mean(t1));%mittlere Zeitabweichung vom Gardner-Algorithmus
    diff_f(j)=mean(df);% mittelere Frequenzabweichung
    frequenz(j+1)=0.95*frequenz(j)+0.05*diff_f(j);%Update der Frequenz für die nächsten 128 Symbole 
    korr_messwert=0.95*korr_messwert+0.05*messwert;%Update der Zeitkorrektur zur Auswahl des besten Signalsamplewert für die nächsten 128 Symbole 
    dt=dt+round(korr_messwert);%dt für neuen Signalsamplewert
 
end;

%Filterkoeffizient für Basisbandfilter
h1=[0.0004   -0.0017   -0.0044   -0.0011    0.0109    0.0171   -0.0028   -0.0369   -0.0359    0.0245    0.0813    0.0401 -0.0831   -0.1273    0.0376    0.3184    0.4585    0.3184    0.0376   -0.1273   -0.0831    0.0401    0.0813    0.0245    -0.0359   -0.0369   -0.0028    0.0171    0.0109   -0.0011   -0.0044   -0.0017    0.0004];
ycc=conv(ybb,h1);%Filterung der Baseband-Samples
lan=round(0.25*length(ycc)-1);
for i=1:lan,%Auswahl des Symbolwertes aus den 4 Basebandsamples und Ermittlung des Frequenzversatzes des gefilterten Signals
    ydd(i)=ycc(4*(i-1)+1);
    if i>1 dfreq(i)=0.5*(angle(ycc(4*(i-1)+2)/ycc(4*(i-1)+1))+angle(ycc(4*(i-1)+1)/ycc(4*(i-1)+0)));end;
end

for i=14001:15000,%Mittelung des Frequenzversatzes nach dem Einschwingen
    deltaf(i-14000)=dfreq(i);
end;

delf=4*mean(deltaf);%Anfangswert zur Frequenzschätzung für PLL-Algorithmus der Datendemodulation

phi_korr=-angle(ydd(15000)+ydd(14999)+ydd(14998));
;%Referenzphase für Datendemodulation, willkürlich gewählt nach Einschwingen
k1=0.04;%Koeffizient für Frequenznachführung der PLL
k2=0.25;%Koeffizient für Phasennachführung der PLL

for i=15001:length(ydd)-2,%PLL für Datendemodulation BPSK
   xx(i)=ydd(i)*exp(sqrt(-1)*phi_korr);%Phasenkorrektur für Symbol ("Einnorden")
   dphi=angle(xx(i));%laufende Phasenabweichung;
   if dphi>pi/2 dphi=dphi-pi;end;if dphi<-pi/2 dphi=dphi+pi;end%Korrektur der Phase, wenn Absolutwert >pi/2   
   delf=delf+k1*dphi;%Update der Frequenzschätzung
   phi_korr=phi_korr-delf-k2*dphi;%Update zur Korrekturphase
   if real(xx(i))>=0 y_data(i)=1;end%Symbolentscheidung
   if real(xx(i))<0 y_data(i)=-1;end%y_data sind die entschiedenen Symbole
end;

bitfehler=0;

for i=15001:length(ydd)-2,
    bitfehler=bitfehler+1;
    if dt==-4 if x_data(i-9)==y_data(i)
        bitfehler=bitfehler-1;end;end;
    if dt==12 if x_data(i-8)==y_data(i)
        bitfehler=bitfehler-1;end;end;
    zzz(i-15000)=bitfehler;
end
fehlerrate=bitfehler/(length(ydd)-15003)


    

