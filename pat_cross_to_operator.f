

*$ CREATE MGDRAW.FOR
*COPY MGDRAW
*                                                                      *
*=== mgdraw ===========================================================*
*                                                                      *
      SUBROUTINE MGDRAW ( ICODE, MREG)

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
      
      !History variables
      
      !Initializing the ID number of the photon
      INTEGER, save :: CURRENT_NCASE = 0
      
      !The maximum number of events in a history
      INTEGER, parameter :: MAX_TEMP = 5000
      
      !Initializing the number of events in a history
      INTEGER, save :: TEMP_INDEX = 0
      
      
      !Crossing variables:

      !Making lists to save the variables 
      INTEGER, save :: TEMP_NCASE(MAX_TEMP) !ID
      INTEGER, save :: TEMP_ICODE(MAX_TEMP) !Interaction number
      REAL*4, save :: TEMP_ETRACK(MAX_TEMP) !Initial energy of photon
      REAL*4, save :: EFINAL(MAX_TEMP)      !Final energy of photon
      REAL*4, save :: TEMP_XPOS(MAX_TEMP)   !X-position of photon
      REAL*4, save :: TEMP_YPOS(MAX_TEMP)   !Y-position of photon
      REAL*4, save :: TEMP_ZPOS(MAX_TEMP)   !Z-position of photon
      INTEGER, save :: TEMP_MREG(MAX_TEMP)  !Region of photon
      
      
      
      !Flag to mark crossing
      LOGICAL, save :: CROSS_FLAG
      DATA CROSS_FLAG / .FALSE. /
      
      
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
*      WRITE (IODRAW) NTRACK, MTRACK, JTRACK, SNGL (ETRACK),
*     &               SNGL (WTRACK)
*      WRITE (IODRAW) ( SNGL (XTRACK (I)), SNGL (YTRACK (I)),
*     &                 SNGL (ZTRACK (I)), I = 0, NTRACK ),
*     &               ( SNGL (DTRACK (I)), I = 1, MTRACK ),
*     &                 SNGL (CTRACK)
*  +-------------------------------------------------------------------*
*  |  Quenching is activated
      IF ( LQEMGD ) THEN
         IF ( MTRACK .GT. 0 ) THEN
            RULLL  = ZERZER
            CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
*            WRITE (IODRAW) ( ( SNGL (DTQUEN (I,JBK)), I = 1, MTRACK ),
*     &                         JBK = 1, NQEMGD )
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

      
      
*
*======================================================================*
*                                                                      *
*     Event End DRAWing:                                               *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY EEDRAW ( ICODE ) !Called at end of each history
      !Using EEDRAW to write all histories at the end of its path
      
      IF (CROSS_FLAG) THEN !Extracting all photon histories that crossed into operator
      
        DO I = 1, TEMP_INDEX !Looping over the number of events in a history
        
           IF (TEMP_ICODE(I) .EQ. 9998) THEN !It is a crossing event, 9998 corresponds to crossing out from patient
           
              !In this IF loop, the exit through the different patient planes are written to the output
              IF (TEMP_ZPOS(I) .EQ. 128.984802) THEN !Top_plane
              
                 WRITE (93, *) TEMP_NCASE(I), TEMP_ETRACK(I),
     & '-9998.0', TEMP_ICODE(I), TEMP_XPOS(I), TEMP_YPOS(I),  
     & TEMP_ZPOS(I), TEMP_MREG(I), 'Pat_cross_top' !Writing the crossing details. -9998.0 is a placeholder since the crossing does not have a final energy 
     
     
              ELSE IF (TEMP_ZPOS(I) .EQ. 90.0199585) THEN !Bottom_plane
              
                 WRITE (93, *) TEMP_NCASE(I), TEMP_ETRACK(I),
     & '-9998.0', TEMP_ICODE(I), TEMP_XPOS(I), TEMP_YPOS(I),  
     & TEMP_ZPOS(I), TEMP_MREG(I), 'Pat_cross_bottom'
     
              ELSE IF (TEMP_XPOS(I) .EQ. 30.0000000) THEN !Plane at the patient's right side
                 WRITE (93, *) TEMP_NCASE(I), TEMP_ETRACK(I),
     & '-9998.0', TEMP_ICODE(I), TEMP_XPOS(I), TEMP_YPOS(I),  
     & TEMP_ZPOS(I), TEMP_MREG(I), 'Pat_cross_right_arm'
     
     
                
              ELSE IF (TEMP_XPOS(I) .EQ. -20.0000000) THEN !Plane at the patient's left side
              
                 WRITE (93, *) TEMP_NCASE(I), TEMP_ETRACK(I),
     & '-9998.0', TEMP_ICODE(I), TEMP_XPOS(I), TEMP_YPOS(I),  
     & TEMP_ZPOS(I), TEMP_MREG(I), 'Pat_cross_left_arm'
     
     
              ELSE IF (TEMP_YPOS(I) .EQ. -45.0000000) THEN !Plane at head
              
                 WRITE (93, *) TEMP_NCASE(I), TEMP_ETRACK(I),
     & '-9998.0', TEMP_ICODE(I), TEMP_XPOS(I), TEMP_YPOS(I),  
     & TEMP_ZPOS(I), TEMP_MREG(I), 'Pat_cross_cranial'
     
        
              ELSE IF (TEMP_YPOS(I) .EQ. 150.0000000) THEN !Plane at feet
              
                 WRITE (93, *) TEMP_NCASE(I), TEMP_ETRACK(I),
     & '-9998.0', TEMP_ICODE(I), TEMP_XPOS(I), TEMP_YPOS(I),  
     & TEMP_ZPOS(I), TEMP_MREG(I), 'Pat_cross_feet'
     
              END IF
     
         
     
           ELSE ! It is a crossing event into the operator
           
              IF (TEMP_ICODE(I) .EQ. 9999) THEN !It is a crossing event, 9999 corresponds to crossing into operator
                 WRITE (93, *) TEMP_NCASE(I), TEMP_ETRACK(I),
     & '-9999.0', TEMP_ICODE(I), TEMP_XPOS(I), TEMP_YPOS(I),  
     & TEMP_ZPOS(I), TEMP_MREG(I), 'Op_cross' 
              
              END IF
             
     
               
              
           END IF
        END DO
        !Reset cross flag for next photon
        CROSS_FLAG = .FALSE.
        TEMP_INDEX = 1 !Resetting the count of events in a history, index set to 1 because arrays start at 1 not 0. 
      END IF
      
       
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
*      WRITE (IODRAW)  0, ICODE, JTRACK, SNGL (ETRACK), SNGL (WTRACK)
*      WRITE (IODRAW)  SNGL (XSCO), SNGL (YSCO), SNGL (ZSCO), SNGL (RULL)
*  +-------------------------------------------------------------------*
*  |  Quenching is activated : calculate quenching factor
*  |  and store quenched energy in DTQUEN(1, jbk)
      IF ( LQEMGD ) THEN
         RULLL = RULL
         CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
*         WRITE (IODRAW) ( SNGL (DTQUEN(1, JBK)), JBK = 1, NQEMGD )
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
*      WRITE (IODRAW) -NCASE, NPFLKA, NSTMAX, SNGL (TKESUM),
*     &                SNGL (WEIPRI)
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
*      WRITE (IODRAW) ( ILOFLK(I), SNGL (TKEFLK(I)+AMHELP(I)),
*     &                 SNGL (WTFLK(I)), SNGL (XFLK (I)),
*     &                 SNGL (YFLK (I)), SNGL (ZFLK (I)),
*     &                 SNGL (TXFLK(I)), SNGL (TYFLK(I)),
*     &                 SNGL (TZFLK(I)), I = 1, NPFLKA )
      RETURN
*
*=========================================================     *
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
      

   
      
      
      ENTRY USDRAW ( ICODE, MREG, XSCO, YSCO, ZSCO )
      
 
      RETURN 
      
      
      
      ENTRY BXDRAW ( ICODE, MREG, NEWREG, XSCO, YSCO, ZSCO ) !Tracking the crossing between regions for the simulation


      IF (NCASE .EQ. CURRENT_NCASE) THEN !This means that this is not the first event for this photon
          TEMP_INDEX = TEMP_INDEX + 1 !Adding to the number of events in the history
      
         IF (MREG .NE. 3 .AND. NEWREG .EQ. 3) THEN !We have a crossing from a none operator region into operator, indicating that something happened before this event
         
             
             !Indication that the photon crossed regions, we flag the event
             CROSS_FLAG = .TRUE.
          
          
             
             !Storing the values in the lists for the crossing of the operator
             TEMP_NCASE(TEMP_INDEX) = NCASE
             TEMP_ICODE(TEMP_INDEX) = 9999
             TEMP_ETRACK(TEMP_INDEX) = ETRACK
             TEMP_XPOS(TEMP_INDEX) = XSCO
             TEMP_YPOS(TEMP_INDEX) = YSCO
             TEMP_ZPOS(TEMP_INDEX) = ZSCO
             TEMP_MREG(TEMP_INDEX) = NEWREG
         END IF
      END IF
      
      IF (MREG > 4 .AND. NEWREG .EQ. 2) THEN !We have a crossing from a patient region into air, indicating crossing out from patient volume
             CURRENT_NCASE = NCASE !This is the first event for this photon, indicating that the photon crosses out from patient region before hitting operator
                           

             TEMP_INDEX = 1 !The count starts at 1, so the first event should be number 1.
             !Storing the values in the lists for the crossing of the patient volume
             TEMP_NCASE(TEMP_INDEX) = NCASE
             TEMP_ICODE(TEMP_INDEX) = 9998
             TEMP_ETRACK(TEMP_INDEX) = ETRACK
             TEMP_XPOS(TEMP_INDEX) = XSCO
             TEMP_YPOS(TEMP_INDEX) = YSCO
             TEMP_ZPOS(TEMP_INDEX) = ZSCO
             TEMP_MREG(TEMP_INDEX) = NEWREG
      END IF
         
      
        
       
*
      RETURN
      
      
    

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
      
      
      
      
      
      
      
      
      
      
      
      
     
 
      
      
     
      
      
      

        
