c     car2.f (version 1.0) May 1998; Christian Fellner
c     Genral program for calculating hybrid vehicle mileage.

c     You may make a copy of this program which you may personally
c     and freely use in its unaltered form.  You may distribute this
c     program subject to the conditions that it be made freely 
c     available and that any duplication of this program must be
c     essentially unaltered and must include this notice.

c     We make no warranties, express or implied, that this program
c     is free of errors or that it will meet the requirements of your
c     application.  The author and publisher disclaim all liability for
c     direct or consequential damages resulting from use of this program.

c     This program calculates the average mileage of a hybrid vehicle.

      PROGRAM CAR2

c     Simplified from original car program.
c        One overpotential fit:  ai^2 + bi + c
c        Electrode open-circuit potential fit:  ax^2 + bx + c
c        State of charge overpotential abjustment:  ax^2 + bx + c
c     No change in open circuit potential correction for overpotential fit. 
c        - Better for general battery and easier to measure.
c     Type 'cat spec# | car2' to run program and read data file.
c        Use specg for general battery.
c     Type 'cat spec# | car2 >file' to run program, read data file,
c     and write output to specified file.

c     Combines a simple vehicle model with a simplified battery model.
c     Works only for design driving cycle. (6 1 minute cycles)

c     Define variable types for battery & vehicle.

      REAL Athick, Cthick, MwA, MwC, Aact, Cact, Adensity, Cdensity
      REAL ULimit, LLimit, OcA1, OcA2, OcA3, OcC1, OcC2, OcC3
      REAL C1, C2, Area, Pbrake, SepStep, SocStep
      REAL Apower1a, Apower1b, Apower2a, Apower2b
      REAL Apower3a, Apower3b, Apower4a, Apower4b
      REAL Vmass, Meng, Emass, Pmass, Bmass
      REAL Peff, Eeff, Geff, Beff, Veng
       
c     Define variable types for battery linear approximations.

c     State of charge overpotential adjustments.
      REAL ScA1, ScA2, ScA3
      REAL ScC1, ScC2, ScC3
      
c     Overpotential fit.
      REAL Op1, Op2, Op3
   
c     Define variables used internal to the program.
      REAL Cpower1, Cpower2, Cpower3, Cpower4
      REAL Apower1, Apower2, Apower3, Apower4
      REAL Bpower1, Bpower2, Bpower3, Bpower4
      REAL Bap1, Bap2, Bap3, Bap4, Bcp1, Bcp2, Bcp3, Bcp4
      REAL Bbp1, Bbp2, Bbp3, Bbp4, Bsp
      REAL Mass, SocA, SocC, Engine, MA, MC, BatOut, BatIn, BatEff
      REAL Delta, Delta1, Delta2, Delta3, Delta4, Xi, Yi, OCP
      REAL Separator, Smin, Smax, Vel1, Vel2, Vel3, Vel4
      REAL I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13
      REAL I14, I15, I16, I17, I18, I19, I20, I21, I22, I23, I24
      REAL V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13
      REAL V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24
      REAL GRAV, AIR, SoccStop, Dump

c     Careful with type declarations on different systems (real).
      INTEGER FARADY, EGAS, Number, t1, t2, t3, t4, t5, t6, tb, ts
      
c     Enter in constant parameter values.
c     FARADY = Faraday's constant; coulombs/equivalent
c     GRAV = gravitational constant; m/s2
c     EGAS = energy content of gasoline; kJ/L
c     AIR = density of air; kg/m3 

      PARAMETER (FARADY = 96487, GRAV = 9.81)
      PARAMETER (EGAS = 31810, AIR = 1.202)

c     Use common statement so can get state of charge factors into subroutine
c     TUNE, FUEL, OVERPOTENTIAL, OPEN easier, must name since more than 1 common.

      COMMON /ad/ ScA1, ScA2, ScA3, ScC1, ScC2, ScC3
      COMMON /gas/ C1, C2, Area, Peff, Eeff, Geff, Veng
      COMMON /op/ Op1, Op2, Op3
      COMMON /oc/ OcA1, OcA2, OcA3, OcC1, OcC2, OcC3

c     Read in variables from data file.
      READ *, Athick
      READ *, Cthick
      READ *, Aact
      READ *, Cact
      READ *, Bmass
      READ *, Pbrake
      READ *, C1
      READ *, C2
      READ *, Area
      READ *, Adensity
      READ *, Cdensity
      READ *, Vmass
      READ *, Apower1a
      READ *, Apower1b
      READ *, Apower2a
      READ *, Apower2b
      READ *, Apower3a
      READ *, Apower3b
      READ *, Apower4a
      READ *, Apower4b      
      READ *, Meng
      READ *, Emass
      READ *, Pmass
      READ *, Peff
      READ *, Eeff
      READ *, Geff
      READ *, Beff
      READ *, MwA
      READ *, MwC
      READ *, ScA1
      READ *, ScA2
      READ *, ScA3
      READ *, ScC1
      READ *, ScC2
      READ *, ScC3
      READ *, Op1
      READ *, Op2
      READ *, Op3
      READ *, OcA1
      READ *, OcA2
      READ *, OcA3
      READ *, OcC1
      READ *, OcC2
      READ *, OcC3
  
c     Print out program input parameters. 
      PRINT *, '***  Vehicle model results.  ***'
      PRINT *, 'Anode electrode thickness (um)=,', Athick
      PRINT *, 'Cathode electrode thickness (um) =,', Cthick
      PRINT *, 'Anode % volume active material =,', Aact
      PRINT *, 'Cathode % volume active material =,', Cact
      PRINT *, 'Battery mass (kg/m2) =,', Bmass
      PRINT *, 'Vehicle rolling drag coefficient =,', C1
      PRINT *, 'Vehicle air drag coefficient =,', C2
      PRINT *, 'Vehicle front surface area (m2) =,', Area
      PRINT *, 'Base vehicle mass (kg) =,', Vmass     
      PRINT *, 'Maximum engine design (kw) =,', Meng
      PRINT *, 'Engine mass (kw/kg) =,', Emass
      PRINT *, 'Passenger mass (kg) =,', Pmass
      PRINT *, 'Powertrain efficiency =,', Peff
      PRINT *, 'Engine thermal efficiency =,', Eeff
      PRINT *, 'Generator efficiency =,', Geff
      PRINT *, 'Engine into battery efficiency =,', Beff
      PRINT *, 'Regenerative braking % =,', Pbrake

c     **********    Begin main body of the program.     **********

c     Initialize variables used in program, set by user for ranges.            
c     Smallest & largest separator area and step size used in program.
      Smin = 85
      Smax = 95
      SepStep = 5
c     Set maximum/minimum state of charge for anode and step size.
      SocA = 0.45
      SoccStop = 0.44
      SocStep = 0.05
c     Enter % of maximum regenerative braking during hard stop.
      Dump = 1.0
      PRINT *, '% braking during hard stop =,', Pbrake*Dump
      PRINT *, ' '
c     Initial engine guess for urban cycle & set variation.
c     Engine maximum during acceleration, at level during cruising,
c        and minimum during braking and rest.
c     Values for Veng range from 0 to 1.
c        0 constant urban cruising engine.
c        1 no engine during braking & stop & double during acceleration.
      Engine = 4500
      Veng = 0.0
c     Set voltage limits on battery.
      LLimit = 1.6
      ULimit = 2.3
c     Cruising velocities in m/s.
      Vel1 = 11.111
      Vel2 = 15.278
      Vel3 = 19.444
      Vel4 = 25.000
c     Driving cycle time segment lengths in seconds.
      t1 = 10
      t2 = 30
      t3 = 12
      t4 = 28
      t5 = 15
      t6 = 25
      tb = 5
      ts = 15       

c     Calculate constants used in program. (mol active in anode & cathode)
c     Units are mol/m2 of separator area.
      MA = Athick*0.000001*Aact*Adensity*1000/MwA
      MC = Cthick*0.000001*Cact*Cdensity*1000/MwC      

c     First big loop, state of charge.
      DO WHILE (SocA .GE. SoccStop)      
c        Fit cathode to same capacity as anode.
         SocC = 1 - SocA*MA/MC
c        Calculate open circuit potential
         OCP = OPEN (SocA, SocC)
c        Print state of charge of electrodes.
         PRINT *, '******   Begin new state of charge loop.   ******'
         PRINT *, ' '
         PRINT *, 'Initial x in anode =,', SocA
         PRINT *, 'Initial y in cathode =,', SocC
         PRINT *, 'Open-circuit cell potential =,', OCP
         PRINT *, ' '

c        Use Xi & Yi to hold initial values for state of charge
         Xi = SocA
         Yi = SocC
         
c        Need to re-initialize separator area for next loop.
         Separator = Smax            
       
c     Second loop, separator area.
      DO WHILE (Separator .GE. Smin)
c        Add in 1.5 factor for battery and engine support.
         Mass = (Meng/Emass)*1.5 + Pmass + Vmass + (Bmass*Separator)*1.5
         PRINT *, 'Separator area (m2) =,', Separator     
         PRINT *, 'Battery mass (no support; kg) =,', Bmass*Separator
         PRINT *, 'Total vehicle mass (kg) =,', Mass    
         CALL POWER (SocA, SocC, Overpot, Separator, LLimit)

c        Calculate vehicle wheel power requirements for each driving segment.
         Apower1 = Apower1a*Mass + Apower1b
         Apower2 = Apower2a*Mass + Apower2b
         Apower3 = Apower3a*Mass + Apower3b
         Apower4 = Apower4a*Mass + Apower4b
         
         Cpower1 = C1*GRAV*Vel1*Mass + 0.5*AIR*C2*Area*Vel1**3
         Cpower2 = C1*GRAV*Vel2*Mass + 0.5*AIR*C2*Area*Vel2**3
         Cpower3 = C1*GRAV*Vel3*Mass + 0.5*AIR*C2*Area*Vel3**3
         Cpower4 = C1*GRAV*Vel4*Mass + 0.5*AIR*C2*Area*Vel4**3
         
c        Need to divide 1/2 by 5 for time. (5 second stop)
         Bpower1 = -0.1*Mass*Vel1**2
         Bpower2 = -0.1*Mass*Vel2**2
         Bpower3 = -0.1*Mass*Vel3**2
         Bpower4 = -0.1*Mass*Vel4**2
         
c     Third loop, solve system to find required engine.      
c     Need to give Delta an initial non-zero value to enter loop.
      Delta = 1

c     Initialize engine counting loop.
      Number = 1

      DO WHILE (ABS(Delta) .GE. 0.01)

c        Counting cycle for engine loop.
         IF (Number .LE. 100) THEN
            Number = Number + 1
         ELSE
            PRINT *, 'I have been in the engine loop too long!'
            STOP
         ENDIF

c        Calculate battery power requirements at leads for each driving segment.
c        For acceleration battery power always assume draining.
c        Battery power positive to supply power.
c        Battery power negative to accept power.

         Bap1 = (Apower1 - Engine*(1+Veng)*Peff)/Geff
         Bap2 = (Apower2 - Engine*(1+Veng)*Peff)/Geff
         Bap3 = (Apower3 - Engine*(1+Veng)*Peff)/Geff
         Bap4 = (Apower4 - Engine*(1+Veng)*Peff)/Geff
         
c        For cruising power must check if draining/charging.
         IF (Cpower1 .GT. Engine*Peff) THEN
            Bcp1 = (Cpower1 - Engine*Peff)/Geff
         ELSE
            Bcp1 = (Cpower1/Peff - Engine)*Beff
         ENDIF
            
         IF (Cpower2 .GT. Engine*Peff) THEN
            Bcp2 = (Cpower2 - Engine*Peff)/Geff
         ELSE
            Bcp2 = (Cpower2/Peff - Engine)*Beff
         ENDIF   
            
         IF (Cpower3 .GT. Engine*Peff) THEN
            Bcp3 = (Cpower3 - Engine*Peff)/Geff
         ELSE
            Bcp3 = (Cpower3/Peff - Engine)*Beff
         ENDIF
            
         IF (Cpower4 .GT. Engine*Peff) THEN
            Bcp4 = (Cpower4 - Engine*Peff)/Geff
         ELSE
            Bcp4 = (Cpower4/Peff - Engine)*Beff 
         ENDIF   

c        Regenerative braking based on energy driving at battery leads.
         Bbp1 = -Engine*(1-Veng)*Beff + Bpower1*Pbrake
         Bbp2 = -Engine*(1-Veng)*Beff + Bpower2*Pbrake
         Bbp3 = -Engine*(1-Veng)*Beff + Bpower3*Pbrake
         Bbp4 = -Engine*(1-Veng)*Beff + Bpower4*Pbrake*Dump
         
         Bsp = -Engine*(1-Veng)*Beff
         
c        Divide all battery powers by separator area to get per unit area.
         Bap1 = Bap1/Separator
         Bap2 = Bap2/Separator
         Bap3 = Bap3/Separator
         Bap4 = Bap4/Separator
         Bcp1 = Bcp1/Separator
         Bcp2 = Bcp2/Separator
         Bcp3 = Bcp3/Separator
         Bcp4 = Bcp4/Separator
         Bbp1 = Bbp1/Separator
         Bbp2 = Bbp2/Separator
         Bbp3 = Bbp3/Separator
         Bbp4 = Bbp4/Separator
         Bsp = Bsp/Separator
         
c        Determine current requirements during each driving segment.
         CALL CURRENT (Bap1,I1,V1,SocA,SocC,Overpot,t1,MA,MC)
         CALL CURRENT (Bcp1,I2,V2,SocA,SocC,Overpot,t2,MA,MC)
         CALL CURRENT (Bbp1,I3,V3,SocA,SocC,Overpot,tb,MA,MC)
         CALL CURRENT (Bsp,I4,V4,SocA,SocC,Overpot,ts,MA,MC)
         CALL CURRENT (Bap3,I5,V5,SocA,SocC,Overpot,t3,MA,MC)
         CALL CURRENT (Bcp3,I6,V6,SocA,SocC,Overpot,t4,MA,MC)
         CALL CURRENT (Bbp3,I7,V7,SocA,SocC,Overpot,tb,MA,MC)
         CALL CURRENT (Bsp,I8,V8,SocA,SocC,Overpot,ts,MA,MC)
         CALL CURRENT (Bap1,I9,V9,SocA,SocC,Overpot,t1,MA,MC)
         CALL CURRENT (Bcp1,I10,V10,SocA,SocC,Overpot,t2,MA,MC)
         CALL CURRENT (Bbp1,I11,V11,SocA,SocC,Overpot,tb,MA,MC)
         CALL CURRENT (Bsp,I12,V12,SocA,SocC,Overpot,ts,MA,MC)
         CALL CURRENT (Bap4,I13,V13,SocA,SocC,Overpot,t5,MA,MC)
         CALL CURRENT (Bcp4,I14,V14,SocA,SocC,Overpot,t6,MA,MC)
         CALL CURRENT (Bbp4,I15,V15,SocA,SocC,Overpot,tb,MA,MC)
         CALL CURRENT (Bsp,I16,V16,SocA,SocC,Overpot,ts,MA,MC)
         CALL CURRENT (Bap2,I17,V17,SocA,SocC,Overpot,t3,MA,MC)
         CALL CURRENT (Bcp2,I18,V18,SocA,SocC,Overpot,t4,MA,MC)
         CALL CURRENT (Bbp2,I19,V19,SocA,SocC,Overpot,tb,MA,MC)
         CALL CURRENT (Bsp,I20,V20,SocA,SocC,Overpot,ts,MA,MC)
         CALL CURRENT (Bap1,I21,V21,SocA,SocC,Overpot,t1,MA,MC)
         CALL CURRENT (Bcp1,I22,V22,SocA,SocC,Overpot,t2,MA,MC)
         CALL CURRENT (Bbp1,I23,V23,SocA,SocC,Overpot,tb,MA,MC)
         CALL CURRENT (Bsp,I24,V24,SocA,SocC,Overpot,ts,MA,MC)         
         
c        Determine if input current = output current.
         Delta1 = (I1 + I9 + I21)*t1 + (I2 + I10 + I22)*t2 
         Delta2 = (I5 + I17)*t3 + (I6 + I18)*t4 +I13*t5 + I14*t6
         Delta3 = (I3 + I7 + I11 + I15 + I19 + I23)*tb
         Delta4 = (I4 + I8 + I12 + I16 + I20 + I24)*ts
         Delta = Delta1 + Delta2 + Delta3 + Delta4

         IF (ABS(Delta) .GE. 0.01) THEN
c           Recalculate engine and try again. Do better than this.
            Engine = Engine + (Delta*OCP*Separator/360)
         ENDIF

c        Set states of charge back to initial loop values before re-enter.
         Socc = Xi
         Socmn = Yi

      ENDDO

c     Calculate battery cycle efficiency, done at battery leads.
c     Efficiency = (Total battery energy output) / (Total battery energy input)

c     Know braking and stopped charge battery & acceleration drains.
      BatIn = ABS((3*Bbp1 + Bbp2 + Bbp3 + Bbp4)*tb + 6*Bsp*ts)
      BatOut = 3*Bap1*t1 + (Bap2 + Bap3)*t3 + Bap4*t5

c     Need to determine with logical statements if cruising charging/draining.
c     Logical statement required for each cruising segment.

      IF (Bcp1 .GE. 0) THEN
         BatOut = BatOut + 3*Bcp1*t2
      ELSE
         BatIn = BatIn + ABS(3*Bcp1*t2)
      ENDIF

      IF (Bcp2 .GE. 0) THEN
         BatOut = BatOut + Bcp2*t4
      ELSE
         BatIn = BatIn + ABS(Bcp2*t4)
      ENDIF

      IF (Bcp3 .GE. 0) THEN
         BatOut = BatOut + Bcp3*t4
      ELSE
         BatIn = BatIn + ABS(Bcp3*t4)
      ENDIF

      IF (Bcp4 .GE. 0) THEN
         BatOut = BatOut + Bcp4*t6
      ELSE
         BatIn = BatIn + ABS(Bcp4*t6)
      ENDIF

      BatEff = BatOut/BatIn      

c     Print out important results of program, after get proper engine & currents.
      PRINT *, 'Battery cycle efficiency =,', BatEff
      PRINT *, 'Base Urban engine size (watts) =,', Engine
      CALL FUEL (Engine, Mass)
      PRINT *, ' '      
c     Print headings for output from subroutine CURRENT.
      PRINT *, 'Average,Average Cell, Battery'
      PRINT *, 'Current,Potential, Power'
      PRINT *, '(A/m2),(volts), (W/m2)'
      PRINT *, I1,',',V1,',',I1*V1
      PRINT *, I2,',',V2,',',I2*V2
      PRINT *, I3,',',V3,',',I3*V3
      PRINT *, I4,',',V4,',',I4*V4
      PRINT *, I5,',',V5,',',I5*V5
      PRINT *, I6,',',V6,',',I6*V6
      PRINT *, I7,',',V7,',',I7*V7
      PRINT *, I8,',',V8,',',I8*V8
      PRINT *, I9,',',V9,',',I9*V9
      PRINT *, I10,',',V10,',',I10*V10
      PRINT *, I11,',',V11,',',I11*V11
      PRINT *, I12,',',V12,',',I12*V12
      PRINT *, I13,',',V13,',',I13*V13
      PRINT *, I14,',',V14,',',I14*V14
      PRINT *, I15,',',V15,',',I15*V15
      PRINT *, I16,',',V16,',',I16*V16
      PRINT *, I17,',',V17,',',I17*V17
      PRINT *, I18,',',V18,',',I18*V18
      PRINT *, I19,',',V19,',',I19*V19
      PRINT *, I20,',',V20,',',I20*V20
      PRINT *, I21,',',V21,',',I21*V21
      PRINT *, I22,',',V22,',',I22*V22
      PRINT *, I23,',',V23,',',I23*V23
      PRINT *, I24,',',V24,',',I24*V24
 
c     Check to see if outside voltage limits. (Has to be better way to do this)
      
      IF ((V1 .LE. LLimit) .OR. (V1 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V2 .LE. LLimit) .OR. (V2 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V3 .LE. LLimit) .OR. (V3 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V4 .LE. LLimit) .OR. (V4 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V5 .LE. LLimit) .OR. (V5 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V6 .LE. LLimit) .OR. (V6 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V7 .LE. LLimit) .OR. (V7 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V8 .LE. LLimit) .OR. (V8 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V9 .LE. LLimit) .OR. (V9 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V10 .LE. LLimit) .OR. (V10 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V11 .LE. LLimit) .OR. (V11 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V12 .LE. LLimit) .OR. (V12 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V13 .LE. LLimit) .OR. (V13 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V14 .LE. LLimit) .OR. (V14 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V15 .LE. LLimit) .OR. (V15 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V16 .LE. LLimit) .OR. (V16 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V17 .LE. LLimit) .OR. (V17 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V18 .LE. LLimit) .OR. (V18 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'
      ELSEIF ((V19 .LE. LLimit) .OR. (V19 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'      
      ELSEIF ((V20 .LE. LLimit) .OR. (V20 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'            
      ELSEIF ((V21 .LE. LLimit) .OR. (V21 .GE. ULimit)) THEN
        PRINT *, 'Voltage out of bounds!, *****'           
      ELSEIF ((V22 .LE. LLimit) .OR. (V22 .GE. ULimit)) THEN
       PRINT *, 'Voltage out of bounds!, *****'         
      ELSEIF ((V23 .LE. LLimit) .OR. (V23 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'            
      ELSEIF ((V24 .LE. LLimit) .OR. (V24 .GE. ULimit)) THEN
         PRINT *, 'Voltage out of bounds!, *****'          
      ELSE
         PRINT *, 'Within voltage limits.'
      ENDIF      
      
      PRINT *, ' ' 
      PRINT *, '*****   End of cycle   *****'
      PRINT *, ' '

c     Adjust separator area & go back into loop.
      Separator = Separator - SepStep
      ENDDO
c     End of separator size loop, second loop.
      
c     Adjust state of charge and go back into loop.
      SocA = SocA - SocStep
      ENDDO
c     End of final loop, state of charge.

c     Exit program.
      STOP
      END     
      
c     ************    Define functions and subroutines.      ************

c     Deterimes overpotential for given current, linear best fit.
      REAL FUNCTION OVERPOTENTIAL (i)
         REAL i 
         
         COMMON /op/ Op1, Op2, Op3

         OVERPOTENTIAL = Op1*i**2 + Op2*i + Op3 
     
      RETURN
      END

c     **************************************************

c     Determines open circuit potential of battery, Bellcore battery.
      REAL FUNCTION OPEN (x, y)
         REAL x, y
         REAL PA, PC

         COMMON /oc/ OcA1, OcA2, OcA3, OcC1, OcC2, OcC3
         
         PA = OcA1*x**2 + OcA2*x + OcA3
         PC = OcC1*y**2 + OcC2*y + OcC3
         
         OPEN = PC - PA
 
      RETURN
      END

c     **************************************************

c     Determines state of charge adjustment for overpotential.
c     Different overpotentials at different states of charge, slight correction.
      REAL FUNCTION TUNE (x, y)
         REAL x, y
         REAL A1, A2
        
         COMMON /ad/ ScA1, ScA2, ScA3, ScC1, ScC2, ScC3

         A1 = ScA1*x**2 + ScA2*x + ScA3
         A2 = ScC1*y**2 + ScC2*y + ScC3
         
         TUNE = (A1)*(A2)
         
      RETURN
      END

c     **************************************************

c     Determines new state of charge after current segment, Faraday's law.
      SUBROUTINE CHARGE (c, x, y, t, ma, mc)
      REAL c, x, y, ma, mc
      INTEGER t
      REAL MolPA, MolPC
      INTEGER FARADY      
      
      PARAMETER (FARADY = 96487)     
      
      MolPA = ma*x
      MolPC = mc*y

      MolPA = MolPA - c*t/FARADY
      MolPC = MolPC + c*t/FARADY

c     New states of charge of electrodes.
      x = MolPA/ma
      y = MolPC/mc       

      RETURN
      END
      
c     **************************************************

c     Determines current for each driving segment.      
      SUBROUTINE CURRENT (p, c, v, x, y, a, t, ma, mc)
      REAL p, c, v, x, y, a, ma, mc
      INTEGER t
      REAL CHANGE, OVER, FIX1, FIX2, FIX
      REAL BatPower1, Batpower2, i1, i2, Error1, Error2
      REAL OCP, xi, yi
      INTEGER Count

c     Guess initial current (P=I*V), Open circuit for initial value.
      OCP = OPEN (x,y)
      c = p/OCP
      Count = 0
      Change = 1
      xi = x
      yi = y      
      
c     FIX1 doesn't need to recalculated since based on initial state of charge.
      FIX1 = TUNE (x, y)            

c     All of this stuff is just for an initial point to begin the iteration.
      OVER = OVERPOTENTIAL (c)
      CALL CHARGE (c, x, y, t, ma, mc)
      FIX2 = TUNE (x, y)
      FIX = (FIX1 + FIX2)/2
               
      OVER = OVER*FIX
      v = OCP - OVER
      BatPower1 = c*v
      Error1 = BatPower1 - p

c     Set i1 = c for initial iteration.
      i1 = c

c     Reset x & y to initial values to begin loop.
      x = xi
      y = yi

c     New guess for second current value in initial iteration.  
      c = c - Error1/v

      DO WHILE (Change .NE. 0)      

c        Add counting section to get out if not converging.
         Count = Count + 1
         IF (Count .GE. 100) THEN
            PRINT *, 'I have been in this current loop too long!'
            STOP
         ENDIF
         
         OVER = OVERPOTENTIAL (c)
 
         CALL CHARGE (c, x, y, t, ma, mc)
         FIX2 = TUNE (x, y)
         FIX = (FIX1 + FIX2)/2
         
         OVER = OVER*FIX
         v = OCP - OVER
         BatPower2 = c*v
         Error2 = BatPower2 - p    
      
         IF (ABS(Error2) .GE. 0.001) THEN
c           Need i2 to hold value of c for i1.
            i2 = c
c           Linear interpolation to guess new current guess.
            c = ((i2 - i1)/(Error2 - Error1))*(0 -  Error1) + i1
            x = xi
            y = yi
c           Change second variables to first variables for next iteration.            
            Error1 = Error2
            i1 = i2
         ELSE
            Change = 0
         ENDIF

      ENDDO

c     Print out details after loop has converged.  Will give too many details.
c      print *, p
c      print *, 'Cell Pot (volts), i (A/m2),  P (w/m2),  x,   y'
c      print *, v,',',c,',',v*c,',',x,',',y
 
      RETURN
      END
      
c     **************************************************

c     Calculates fuel efficiency
      SUBROUTINE FUEL (e, m)
      REAL e, m
      REAL Vh, Hpower, Heng, Hkml, Udist, Ukml, Ekml
      REAL Eave1a, Eave1b, Eave1, Eave2, Eave
      REAL AIR, GRAV
      INTEGER EGAS, t1, t2, t3, t4, t5, t6, tb, ts

      PARAMETER (EGAS = 31810, AIR = 1.202, GRAV = 9.81)
      
      COMMON /gas/ C1, C2, Area, Peff, Eeff, Geff, Veng

c     Driving cycle time segment lengths in seconds.
      t1 = 10
      t2 = 30
      t3 = 12
      t4 = 28
      t5 = 15
      t6 = 25
      tb = 5
      ts = 15

c     Highway cruising speed = 27.778 m/s
      Vh = 27.778    
      
c     No conversion factor required to get in km/L
      Hpower = C1*GRAV*Vh*m + 0.5*AIR*C2*Area*Vh**3
      Heng = Hpower/Peff	    
      Hkml = (Vh/Heng)*EGAS*Eeff
      Hbat = Hpower/Geff

c     Urban distance travelled ~ 3535 meters in 6 minutes
      Udist = 3535
c     Calculate average urban engine since variable design possible.
      Eave1a = (1+Veng)*(3*t1+2*t3+t5)+(3*t2+2*t4+t6)
      Eave1b = (1-Veng)*6*(tb+ts)
      Eave1 = (Eave1a + Eave1b)*e
      Eave2 = 3*(t1+t2)+2*(t3+t4)+t5+t6+6*(tb+ts)
      Eave = Eave1/Eave2
      Ukml = (Udist/(360*Eave))*EGAS*Eeff
      
c     Equivalent miles per gallon: 55% urban driving & 45% highway driving
      Ekml = 0.55*Ukml + 0.45*Hkml
  
c     Print out mileage estimates
      PRINT *, 'Engine variability =,', Veng
      PRINT *, 'Average urban engine (watts) =,', Eave
      PRINT *, 'Highway cruising engine (watts) =,', Heng
      PRINT *, 'ZEV battery power (watts) =,', Hbat
      PRINT *, 'Highway mileage (km/L) =,', Hkml
      PRINT *, 'Urban mileage (km/L) =,', Ukml
      PRINT *, 'Average mileage (km/L) =,', Ekml

c     Multiple by 0.6214/0.26417 = 2.35227 to convert from km/L to mpg
      PRINT *, 'Highway mileage (mpg) =,', Hkml*2.35227
      PRINT *, 'Urban mileage (mpg) =,', Ukml*2.35227
      PRINT *, 'Average mileage (mpg) =,', Ekml*2.35227      

      RETURN
      END
            
c     **************************************************

c     Determines approximate maximum battery power available.  (10 seconds)
      SUBROUTINE POWER (x, y, a, s, l)
         REAL x, y, a, s, l
         REAL C, Pnew, Pold, OCP, LESS, VOLT
         
c        Initialize variables and get into loop.
         C = 0
         OCP = OPEN (x, y)
         Pnew = 5
         Pold = 0
         VOLT = 4
         
c        Loop to determine maximum power of battery with voltage restriction.
         DO WHILE ((Pnew .GT. Pold) .AND. (VOLT .GE. l))
           Pold = Pnew
           C = C + 1
           LESS = OVERPOTENTIAL (C)
           VOLT = OCP - LESS
           Pnew = C*VOLT*s                  
         ENDDO
         
         PRINT *, ' '
         PRINT *, 'Voltage at maximum power (volts) =,', VOLT
         PRINT *, 'Current at maximum power (A/m2) =,', C
         PRINT *, 'Maximum 10 second power (watts) =,', Pnew
         PRINT *, ' '
         
c        Loop to determine maximum power of battery w/o voltage restriction.
         DO WHILE (Pnew .GT. Pold)
           Pold = Pnew
           C = C + 1
           LESS = OVERPOTENTIAL (C)
           VOLT = OCP - LESS
           Pnew = C*VOLT*s                  
         ENDDO
         
         PRINT *, 'Voltage at maximum power (volts) =,', VOLT
         PRINT *, 'Current at maximum power (A/m2) =,', C
         PRINT *, 'Maximum 10 second power (watts) =,', Pnew
         PRINT *, ' '
      
      RETURN
      END
