*$ CREATE MGDRAW.FOR
*COPY MGDRAW
*                                                                      *
*=== mgdraw ===========================================================*
*                                                                      *
      SUBROUTINE MGDRAW ( ICODE, MREG )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2021      by   Alfredo Ferrari & Paola Sala   *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     MaGnetic field trajectory DRAWing: actually this entry manages   *
*                                        all trajectory dumping for    *
*                                        drawing                       *
*                                                                      *
*     Created on   01 March 1990   by        Alfredo Ferrari           *
*                                              INFN - Milan            *
*                                                                      *
*     Last change   03-Apr-21      by   Alfredo Ferrari &  Paola Sala  *
*                                          Private        INFN - Milan *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(CASLIM)'
      INCLUDE '(COMPUT)'
      INCLUDE '(SOURCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(GENSTK)'
      INCLUDE '(MGDDCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(QUEMGD)'
      INCLUDE '(SUMCOU)'
      INCLUDE '(TRACKR)'
*
      DIMENSION DTQUEN ( MXTRCK, MAXQMG ), AMHELP ( MFSTCK )
*
      CHARACTER*20 FILNAM
      LOGICAL LFCOPE
      SAVE LFCOPE
      DATA LFCOPE / .FALSE. /
*
*----------------------------------------------------------------------*
*                                                                      *
*     Icode = 1: call from Kaskad                                      *
*     Icode = 2: call from Emfsco                                      *
*     Icode = 3: call from Kasneu                                      *
*     Icode = 4: call from Kashea                                      *
*     Icode = 5: call from Kasoph                                      *
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
      !WRITE (IODRAW) NTRACK, MTRACK, JTRACK, SNGL (ETRACK),
      !&               SNGL (WTRACK)
      !WRITE (IODRAW) ( SNGL (XTRACK (I)), SNGL (YTRACK (I)),
      !&                 SNGL (ZTRACK (I)), I = 0, NTRACK ),
      !&               ( SNGL (DTRACK (I)), I = 1, MTRACK ),
      !&                 SNGL (CTRACK)
*  +-------------------------------------------------------------------*
*  |  Quenching is activated
      IF ( LQEMGD ) THEN
         IF ( MTRACK .GT. 0 ) THEN
            RULLL  = ZERZER
            CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
            WRITE (IODRAW) ( ( SNGL (DTQUEN (I,JBK)), I = 1, MTRACK ),
     &                         JBK = 1, NQEMGD )
         END IF
      END IF
*  |  End of quenching
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     Boundary-(X)crossing DRAWing:                                    *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             19: boundary crossing                                    *
*     Icode = 2x: call from Emfsco                                     *
*             29: boundary crossing                                    *
*     Icode = 3x: call from Kasneu                                     *
*             39: boundary crossing                                    *
*     Icode = 4x: call from Kashea                                     *
*             49: boundary crossing                                    *
*     Icode = 5x: call from Kasoph                                     *
*             59: boundary crossing                                    *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY BXDRAW ( ICODE, MREG, NEWREG, XSCO, YSCO, ZSCO )
*
      RETURN
*
*======================================================================*
*                                                                      *
*     Event End DRAWing:                                               *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY EEDRAW ( ICODE )
*
      RETURN
*
*======================================================================*
*                                                                      *
*     ENergy deposition DRAWing:                                       *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             10: elastic interaction recoil                           *
*             11: inelastic interaction recoil                         *
*             12: stopping particle                                    *
*             13: pseudo-neutron deposition                            *
*             14: escape                                               *
*             15: time kill                                            *
*             16: recoil from (heavy) bremsstrahlung                   *
*     Icode = 2x: call from Emfsco                                     *
*             20: local energy deposition (i.e. photoelectric)         *
*             21: below threshold, iarg=1                              *
*             22: below threshold, iarg=2                              *
*             23: escape                                               *
*             24: time kill                                            *
*     Icode = 3x: call from Kasneu                                     *
*             30: target recoil                                        *
*             31: below threshold                                      *
*             32: escape                                               *
*             33: time kill                                            *
*     Icode = 4x: call from Kashea                                     *
*             40: escape                                               *
*             41: time kill                                            *
*             42: delta ray stack overflow                             *
*     Icode = 5x: call from Kasoph                                     *
*             50: optical photon absorption                            *
*             51: escape                                               *
*             52: time kill                                            *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY ENDRAW ( ICODE, MREG, RULL, XSCO, YSCO, ZSCO )
*
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
      !WRITE (IODRAW)  0, ICODE, JTRACK, SNGL (ETRACK), SNGL (WTRACK)
      !WRITE (IODRAW)  SNGL (XSCO), SNGL (YSCO), SNGL (ZSCO), SNGL (RULL)
*  +-------------------------------------------------------------------*
*  |  Quenching is activated : calculate quenching factor
*  |  and store quenched energy in DTQUEN(1, jbk)
      IF ( LQEMGD ) THEN
         RULLL = RULL
         CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
         WRITE (IODRAW) ( SNGL (DTQUEN(1, JBK)), JBK = 1, NQEMGD )
      END IF
*  |  end quenching
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     SOurce particle DRAWing:                                         *
*                                                                      *
*======================================================================*
*
      ENTRY SODRAW
*
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
      !WRITE (IODRAW) -NCASE, NPFLKA, NSTMAX, SNGL (TKESUM),
      !&                SNGL (WEIPRI)
*  +-------------------------------------------------------------------*
*  |  Loop on main stack particle(s):
      DO 2000 I = 1, NPFLKA
*  |  +----------------------------------------------------------------*
*  |  |  (Radioactive) isotope: note that a fictitious mass is setup
*  |  |  so as to result in -Tkeflk printed:
         IF ( ABS (ILOFLK (I)) .GE. KPIOMN .AND. LRADDC (I) ) THEN
            IARES  = MOD ( ILOFLK (I), 100000  )  / 100
            IZRES  = MOD ( ILOFLK (I), 10000000 ) / 100000
            IF ( IZRES .LT. 70 .AND. IARES .GT. 200 ) IZRES = IZRES +100
            IISRES = ILOFLK (I) / 10000000
            AMHELP (I) = -TWOTWO * TKEFLK (I)
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  Patch for heavy ions:
         ELSE IF ( ABS (ILOFLK (I)) .GE. KPIOMN ) THEN
            IONID  = ILOFLK (I)
            ILVION = ILVSTK (I)
            EXCION = EEXSTK (I)
            TMNION = TMNSTK (I)
            CALL DCEXIO ( IONID, ILVION, EXCION, TMNION )
            AMHELP (I) = AMNHEA (-IONID) + EEXSTK (I)
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  Patch for heavy ions: ???
         ELSE IF ( ILOFLK (I) .LT. -6 ) THEN
            EXCION = EEXSTK (I)
            AMHELP (I) = AMNHEA (-ILOFLK(I)) + EEXSTK (I)
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |
         ELSE
            AMHELP (I) = AM (ILOFLK(I))
         END IF
*  |  |
*  |  +----------------------------------------------------------------*
 2000 CONTINUE
*  |
*  +-------------------------------------------------------------------*
      !WRITE (IODRAW) ( ILOFLK(I), SNGL (TKEFLK(I)+AMHELP(I)),
      !&                 SNGL (WTFLK(I)), SNGL (XFLK (I)),
      !&                 SNGL (YFLK (I)), SNGL (ZFLK (I)),
      !&                 SNGL (TXFLK(I)), SNGL (TYFLK(I)),
      !&                 SNGL (TZFLK(I)), I = 1, NPFLKA )
      RETURN
*
*======================================================================*
*                                                                      *
*     USer dependent DRAWing:                                          *
*                                                                      *
*     Icode =  99: call from Doiosp, ion splitting secondaries         *
*     Icode = 10x: call from Kaskad                                    *
*             100: elastic   interaction secondaries                   *
*             101: inelastic interaction secondaries                   *
*             102: particle decay  secondaries                         *
*             103: delta ray  generation secondaries                   *
*             104: pair production secondaries                         *
*             105: bremsstrahlung  secondaries                         *
*             106: de-excitation in flight secondaries                 *
*             110: radioactive decay products                          *
*     Icode = 20x: call from Emfsco                                    *
*             208: bremsstrahlung secondaries                          *
*             210: Moller secondaries                                  *
*             212: Bhabha secondaries                                  *
*             214: in-flight annihilation secondaries                  *
*             215: annihilation at rest   secondaries                  *
*             217: pair production        secondaries                  *
*             219: Compton scattering     secondaries                  *
*             221: photoelectric          secondaries                  *
*             225: Rayleigh scattering    secondaries                  *
*             237: mu pair production     secondaries                  *
*     Icode = 30x: call from Kasneu                                    *
*             300: interaction secondaries                             *
*     Icode = 40x: call from Kashea                                    *
*             400: delta ray  generation secondaries                   *
*                                                                      *
*  For all interactions secondaries are put on GENSTK common (kp=1,np) *
*  but for KASHEA delta ray generation where only the secondary elec-  *
*  tron is present and stacked on FLKSTK common for kp=npflka          *
*                                                                      *
*  !!! For optical photon production events, please refer to the  !!!  *
*  !!! pshckp, ustckv (Cerenkov), pshscp, ustscn (Scintillation)  !!!  *
*  !!! user routines                                              !!!  *
*                                                                      *
*======================================================================*
*
       !Tracking code starts here
      ENTRY USDRAW ( ICODE, MREG, XSCO, YSCO, ZSCO ) !Tracking the interactions happening in the simulation
 

      !Only Compton, Rayleigh and PE are recorded
      IF (MREG .EQ. 3) THEN !Only recording interactions inside the phantom

         IF (Icode .EQ. 219) THEN ! Checking if an interaction is a Compton scattering event         
           DO IP = 1, NP !Looping over number of particles
              IF (KPART(IP) .EQ. 7) THEN !KPART corresponds to which type of particle, 7 for photon
                  EFINAL=Tki(IP) !To obtain final energy of interactions
              END IF
           END DO
           WRITE(49,*) NCASE, ETRACK, EFINAL, 'Compton', XSCO, YSCO, 
     &  ZSCO, MREG !Writing data to output file
     
         ELSE IF (Icode .EQ. 225) THEN !Checking if an interaction is a Rayleigh scattering
           DO IP = 1, NP
              IF (KPART(IP) .EQ. 7) THEN
                  EFINAL=Tki(IP)
              END IF
           END DO
           WRITE(49,*) NCASE, ETRACK, EFINAL, 'Rayleigh', XSCO, YSCO, 
     &  ZSCO, MREG
     
         ELSE IF (Icode .EQ. 221) THEN !Checking if an interaction is a Photoelectric effect
           EFINAL=0 !It will have 0 keV if undergoing PE effect
           WRITE(49,*) NCASE, ETRACK, EFINAL, 'Photoelectric', XSCO, 
     &  YSCO, ZSCO, MREG
     
         END IF
      END IF
      
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
*      END IF
* No output by default:
      RETURN
*=== End of subrutine M ==========================================*
      END

