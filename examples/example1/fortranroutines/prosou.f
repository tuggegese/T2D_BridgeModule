!                   *****************
                    SUBROUTINE PROSOU
!                   *****************
!
     &(FU,FV,SMH,    UN,VN,HN,GRAV,
     & FAIR,WINDX,WINDY,VENT,HWIND,CORIOL,FCOR,
     & SPHERI,YASMH,YASMO,COSLAT,SINLAT,AT,LT,DT,
     & NREJET,NREJEU,DSCE,ISCE,T1,MESH,MSK,MASKEL,
     & MAREE,MARDAT,MARTIM,PHI0,OPTSOU,
     & COUROU,NPTH,VARCL,NVARCL,VARCLA,UNSV2D,
     & FXWAVE,FYWAVE,RAIN,RAIN_MMPD,PLUIE,
     & T2D_FILES,T2DBI1,BANDEC,
     & NBUSE,ENTBUS,SORBUS,DBUS,UBUS,VBUS,
     & TYPSEUIL,NWEIRS,N_NGHB_W_NODES,
     & MAXSCE,NREG,PT_IN_POLY,TNP,AREA_P,EQUA,CF)
!
!***********************************************************************
! TELEMAC2D   V8P4
!***********************************************************************
!
!brief    PREPARES THE SOURCE TERMS IN THE CONTINUITY EQUATION
!+                AND IN THE DYNAMIC EQUATIONS. ARE TAKEN INTO ACCOUNT :
!+
!+              - WIND
!+
!+              - CORIOLIS FORCE
!+
!+              - TIDAL FORCE
!+
!+              - SOURCES AND SINKS
!+
!+              - WEIRS (IF TYPSEUIL=2)
!code
!+    RESPECTIVE TERMS ARE:
!+    ==========================
!+
!+     * WIND
!+       ---------
!+                                 1                         2      2
!+                FU           =  --- * F    * U    * SQRT( U    + V   )
!+                  VENT           H     AIR    AIR          AIR    AIR
!+
!+                                 1                         2      2
!+                FV           =  --- * F    * V    * SQRT( U    + V   )
!+                  VENT           H     AIR    AIR          AIR    AIR
!+
!+           WHERE :
!+                  UAIR   :  WIND VELOCITY ALONG X
!+                  VAIR   :  WIND VELOCITY ALONG Y
!+                  FAIR   :  AIR FRICTION COEFFICIENT
!+
!+     * CORIOLIS FORCE
!+       ---------------------
!+
!+                FU           =  + FCOR * V
!+                  CORIOLIS
!+
!+                FV           =  - FCOR * U
!+                  CORIOLIS
!+
!+           WHERE :
!+                  U       :  FLOW VELOCITY ALONG X
!+                  V       :  FLOW VELOCITY ALONG Y
!+                  FCOR    :  CORIOLIS PARAMETER
!
!note     BOTTOM FRICTION IS TAKEN INTO ACCOUNT IN THE PROPAGATION
!+         THROUGH CALL TO FROTXY, IT IS SEMI-IMPLICIT.
!note  IF SOURCES OR SINKS TERMS ARE ADDED TO THE CONTINUITY EQUATION,
!+         IT IS IDENTIFIED WITH VARIABLE YASMH (SET TO TRUE).
!note  SOURCE TERMS FU AND FV ARE FIRST COMPUTED IN P1.
!+         THEY ARE THEN EXTENDED TO QUASI-BUBBLE IF REQUIRED.
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (LNHE)
!+        20/02/2012
!+        V6P2
!+   Rain-evaporation added (after initial code provided by O. Boutron,
!+   Tour du Valat and O. Bertrand, Artelia-group).
!
!history  C.COULET (ARTELIA)
!+        23/05/2012
!+        V6P2
!+   Modification for culvert management
!+   Addition of Tubes management
!
!history  R. KOPMANN (EDF R&D, LNHE)
!+        16/04/2013
!+        V6P3
!+   Adding the file format in calls to FIND_IN_SEL.
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        21/05/2013
!+        V6P3
!+   Possibility of negative depths taken into account when limiting
!+   evaporation on dry zones.
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        07/06/2013
!+        V6P3
!+   Modification for new treatment of weirs
!
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Secondary flow correction: compute the secondary
!+   stress term \tau_s and secondary source terms S_x, S_y.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history J.-P. TRAVERT (EDF R&D, LNHE)
!+       07/06/2022
!+       V8P4
!+       Modification to add new infiltration model
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| BANDEC         |-->| IF YES, TIDAL FLATS OR DRY ZONES
!| CF             |<->| ADIMENSIONNAL FRICTION
!| CORIOL         |-->| IF YES, CORIOLIS FORCE
!| COSLAT         |-->| COSINUS OF LATITUDE (SPHERICAL COORDINATES)
!| COUROU         |-->| IF YES, WAVE DRIVEN CURRENTS TAKEN INTO ACCOUNT
!| DSCE           |-->| DISCHARGE OF POINT SOURCES
!| DT             |-->| TIME STEP IN SECONDS
!| ENTBUS         |-->| INDICES OF ENTRY OF TUBES IN GLOBAL NUMBERING
!| FAIR           |<->| FRICTION COEFFICIENT FOR WIND
!| FCOR           |-->| CORIOLIS PARAMETER
!| FU             |<->| SOURCE TERMS ON VELOCITY U
!| FV             |<->| SOURCE TERMS ON VELOCITY V
!| FXWAVE         |<->| FORCING OF WAVES ALONG X
!| FYWAVE         |<->| FORCING OF WAVES ALONG Y
!| GRAV           |-->| GRAVITY
!| HN             |-->| DEPTH AT TIME T(N)
!| HWIND          |-->| MINIMUM DEPTH FOR TAKING WIND INTO ACCOUNT
!| ISCE           |-->| NEAREST POINTS TO SOURCES
!| LT             |-->| TIME STEP NUMBER
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MAREE          |-->| IF YES, TAKES THE TIDAL FORCE INTO ACCOUNT
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBUSE          |-->| NUMBER OF TUBES
!| N_NGHB_W_NODES |-->| NUMBER OF NEIGHBOURS PROCESSORS IN CASE OF // (FOR WEIRS NODES)
!| NPTH           |-->| RECORD NUMBER IN THE WAVE CURRENTS FILE
!| NREJET         |-->| NUMBER OF POINT SOURCES
!| NREJEU         |-->| NUMBER OF POINT SOURCES WITH GIVEN VELOCITY
!|                |   | IF NREJEU=0 VELOCITY OF SOURCES IS TAKEN EQUAL
!|                |   | TO VELOCITY.
!| NDGA1,NDGB1    |-->| INDICES OF POINTS OF WEIRS
!| NVARCL         |-->| NUMBER OF CLANDESTINE VARIABLES
!| NWEIRS         |-->| NUMBER OF WEIRS
!| OPTSOU         |-->| OPTION FOR THE TREATMENT OF SOURCES
!| PHI0           |-->| LONGITUDE OF ORIGIN POINT
!| PLUIE          |-->| BIEF_OBJ STRUCTURE WITH RAIN OR EVAPORATION.
!| RAIN           |-->| IF YES, RAIN OR EVAPORATION TAKEN INTO ACCOUNT
!| RAIN_MMPD      |-->| RAIN OR EVAPORATION IN MM PER DAY
!| SINLAT         |-->| SINUS OF LATITUDE (SPHERICAL COORDINATES)
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SORBUS         |-->| INDICES OF TUBES EXITS IN GLOBAL NUMBERING
!| SPHERI         |-->| IF TRUE : SPHERICAL COORDINATES
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2D_FILES      |-->| BIEF_FILE STRUCTURE WITH ALL TELEMAC-2D FILES
!| T2D_BI1        |-->| RANK OF BINARY FILE 1
!| TYPSEUIL       |-->| TYPE OS WEIRS (ONLY TYPSEUIL=2 IS MANAGE HERE)
!| UBUS           |-->| VELOCITY U AT TUBE EXTREMITY
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| VBUS           |-->| VELOCITY V AT TUBE EXTREMITY
!| VARCL          |<->| BLOCK OF CLANDESTINE VARIABLES
!| VARCLA         |-->| NAMES OF CLANDESTINE VARIABLES
!| VENT           |-->| IF YES, WIND IS TAKEN INTO ACCOUNT
!| WINDX          |-->| FIRST COMPONENT OF WIND VELOCITY
!| WINDY          |-->| SECOND COMPONENT OF WIND VELOCITY
!| YASMH          |<->| IF TRUE SMH IS TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!     FOR SEEING OTHER VARIABLES IN DECLARATIONS_TELEMAC2D:
      USE DECLARATIONS_TELEMAC2D, ONLY : WNODES_PROC,WNODES,U,V,H,
     &                                   RAIN_HDUR,CHESTR,KARMAN,
     &                                   SECCURRENTS,NTRAC,SEC_R,CN,
     &                                   SEC_TAU,T2,T3,T7,ROEAU,S,FC,F0,
     &                                   IELMU,T,ACCROF,RUNOFFOPT,AMC,
     &                                   T2DFO2,ZF,ZFSLOP,FAIRACCU,KS,
     &                                   PROSOU_DEJALU,KFROT,T4,T5,T6,
     &                                   ACCINF,T8,T9,T10,T11,UMEANM,
     &                                   PROSOU_DEJALU,HABRIDGE,HAGEO,
     &                                   BM1,BM2,CTRLSC,CV1,HPROP,
     &                                   MSKSEC,NCP,NODESTRING,NBRIDGES,
     &                                   X,Y,NODELOCAL,DISTTRI,
     &                                   BNEIGHLOC,NPAIRLOC,NODUMEANLOC
      USE INTERFACE_TELEMAC2D, EX_PROSOU => PROSOU
      USE M_COUPLING_ESTEL3D
      USE INTERFACE_HERMES
      USE INTERFACE_PARALLEL, ONLY : P_SUM, P_MAX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     WORKING ARRAYS
!
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1
!
!-----------------------------------------------------------------------
!
!     VECTORS
!
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: FU,FV,SMH,FXWAVE,FYWAVE
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: MASKEL,UN,VN,HN,UNSV2D
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: WINDX,WINDY,COSLAT,SINLAT
!
!-----------------------------------------------------------------------
!
!     MESH STRUCTURE
!
      TYPE(BIEF_MESH)  , INTENT(INOUT) :: MESH
!
!-----------------------------------------------------------------------
!
      INTEGER          , INTENT(IN)    :: NVARCL,LT,NREJET,NREJEU,OPTSOU
      INTEGER          , INTENT(IN)    :: NPTH,T2DBI1
      INTEGER          , INTENT(IN)    :: MARDAT(3),MARTIM(3)
      INTEGER          , INTENT(IN)    :: ISCE(NREJET)
      DOUBLE PRECISION , INTENT(IN)    :: HWIND,AT,FCOR
      DOUBLE PRECISION , INTENT(INOUT) :: FAIR
      DOUBLE PRECISION , INTENT(IN)    :: DSCE(NREJET)
      DOUBLE PRECISION , INTENT(IN)    :: GRAV,PHI0,RAIN_MMPD,DT
      CHARACTER(LEN=32), INTENT(IN)    :: VARCLA(NVARCL)
      CHARACTER(LEN=20), INTENT(IN)    :: EQUA
      LOGICAL          , INTENT(IN)    :: VENT,MAREE,CORIOL,SPHERI,MSK
      LOGICAL          , INTENT(IN)    :: COUROU,RAIN,BANDEC
      LOGICAL          , INTENT(INOUT) :: YASMH,YASMO
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: VARCL,PLUIE,CF
      TYPE(BIEF_FILE)  , INTENT(IN)    :: T2D_FILES(*)
!
      INTEGER          , INTENT(IN)    :: NBUSE
      INTEGER          , INTENT(IN)    :: ENTBUS(NBUSE),SORBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: DBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: UBUS(2,NBUSE),VBUS(2,NBUSE)
!
      INTEGER          , INTENT(IN)    :: TYPSEUIL,NWEIRS,N_NGHB_W_NODES
      INTEGER          , INTENT(IN)    :: NREG,MAXSCE
      INTEGER          , INTENT(IN)    :: TNP(NREG)
      INTEGER          , INTENT(IN)    :: PT_IN_POLY(MAXSCE,*)
      DOUBLE PRECISION , INTENT(IN)    :: AREA_P(NREG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I,IELM1,NPOIN,IR,ERR,NP,K,II,IREG,TTL,J,I1,I2
!
      DOUBLE PRECISION PI,WROT,WD,ATH,RAIN_MPS,SURDT,XX,ROAIR
      DOUBLE PRECISION NX,NY,UN1,UN2,FLXBRIDGE, SUR6, DELTAH, DIST
      DOUBLE PRECISION ANGLE, ELEVUS, ELEVDS,ELEV1,ELEV2, COUNT
      DOUBLE PRECISION AOUTDS, AINDS, AOUTUS, AINUS, ABRIUS, ABRIDS
      DOUBLE PRECISION V1,V3, MUWEI, BLCO, BLEX, H1, H2, CMOD
      DOUBLE PRECISION UMEAN, VMEAN, UMI, VMI, VOUT, VIN
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SEGLENG
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SEGQ,GRADHY
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: GRADH,GRADHX
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: GNODX,GNODY
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODEBRI
!
      CHARACTER(LEN=32) NOMX,NOMY
      CHARACTER(LEN=8) :: FFORMAT
      INTEGER :: FILE_ID, IREC
      LOGICAL OKX,OKY
!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
!  EXTRACTS X COORDINATES, NUMBER OF POINTS P1
!                          AND P1 ELEMENT OF THE MESH
!-----------------------------------------------------------------------
!
      IELM1 = MESH%X%ELM
      NPOIN = MESH%NPOIN
!
!-----------------------------------------------------------------------
!  INITIALISES
!-----------------------------------------------------------------------
!
      CALL CPSTVC(UN,FU)
      CALL CPSTVC(VN,FV)
      CALL OS( 'X=0     ' , X=FU )
      CALL OS( 'X=0     ' , X=FV )
!
!=======================================================================
!
! Bridges source terms, SG, CD
!
!=======================================================================
!
!     Compute discharge through cross section
!
      SUR6=1.D0/6.D0

!     Bridge loop
      DO I=1,NBRIDGES
!         Compute average flux through the bridge for 1D loss
!         computation
          FLXBRIDGE = 0D0
!         Allocate helper arrays
!         Fluxes through faces
          allocate(SEGQ(SIZE(NODESTRING(I)%VEC) / 2))
!         Distances between the nodes
          allocate(SEGLENG(SIZE(NODESTRING(I)%VEC) / 2))
!         Local nodelist of bridges
          allocate(NODEBRI(SIZE(NODESTRING(I)%VEC) / 2 + 1))
          NODEBRI(:) = 0
!         head loss gradient on the nodes in x direction
          allocate(GNODX(SIZE(NODESTRING(I)%VEC) / 2 + 1))
!         head loss gradient on the nodes in y direction
          allocate(GNODY(SIZE(NODESTRING(I)%VEC) / 2 + 1))
          GNODX(:) = 0.D0
          GNODY(:) = 0.D0
!         Compute average flux
          DO J=1,SIZE(NODELOCAL(I)%VEC)/2
!             The local arrays come with zeros
!             that have to be excluded
              I1 = NODELOCAL(I)%VEC(1,J)
              I2 = NODELOCAL(I)%VEC(2,J)
              IF (I1.NE.0 .and. I2.NE.0) THEN
                  NX = (Y(I1)-Y(I2))
                  NY = (X(I2)-X(I1))
                  H1 = H%R(I1)
                  H2 = H%R(I2)
                  UN1= U%R(I1)*NX + V%R(I1)*NY
                  UN2= U%R(I2)*NX + V%R(I2)*NY
                  FLXBRIDGE = FLXBRIDGE
     &                  + ((H1+H2)*(UN1+UN2)+H2*UN2+H1*UN1)*SUR6
!                 Add flux
                  SEGQ(J) = ((H1+H2)*(UN1+UN2)+H2*UN2+H1*UN1)*SUR6
!                 Add distances
                  DIST = ((X(I2)-X(I1))**2.+(Y(I2)-Y(I1))**2.)**(1./2.)
                  SEGLENG(J) = DIST
!                 Add nodes
                  NODEBRI(J) = I1
                  NODEBRI(J+1) = I2 
              ENDIF
          ENDDO
!         Merge flux from processors
          IF (NCSIZE.GT.1) FLXBRIDGE = P_SUM(FLXBRIDGE)          
!         Compute head loss
!         Determine average crosssectional elevation US and DS
!         Loop over nodepairs and determine US and DS
          ELEVUS = 0D0
          ELEVDS = 0D0
          UMEAN = 0D0
          VMEAN = 0D0
          COUNT = 0D0
!         Compute average waterlevels
          DO J = 1, SIZE(NPAIRLOC(I)%VEC) / 2
              I1 = NPAIRLOC(I)%VEC(1,J)
              I2 = NPAIRLOC(I)%VEC(2,J)
              IF (I1.NE.0) THEN
                  H1 = H%R(I1)
              ELSE
                  H1 = 0D0
              ENDIF
              IF (I2.NE.0) THEN
                  H2 = H%R(I2)
              ELSE
                  H2 = 0D0
              ENDIF
              IF (NCSIZE.GT.1) H1 = P_MAX(H1)
              IF (NCSIZE.GT.1) H2 = P_MAX(H2)
!             Check waterdepth
              IF ((H1+H2) .GT. 0.02D0) THEN
                  IF (I1.NE.0) THEN
                      ELEV1 = ZF%R(I1) + H%R(I1)
                      UMI = U%R(I1)
                      VMI = V%R(I1)
                  ELSE
                      ELEV1 = 0.D0
                      UMI = 0.D0
                      VMI = 0.D0
                  ENDIF
                  IF (I2.NE.0) THEN
                      ELEV2 = ZF%R(I2) + H%R(I2)
                  ELSE
                      ELEV2 = 0.D0
                  ENDIF
                  IF (NCSIZE.GT.1) ELEV1 = P_MAX(ELEV1)
                  IF (NCSIZE.GT.1) ELEV2 = P_MAX(ELEV2)
                  IF (NCSIZE.GT.1) UMI = P_SUM(UMI)
                  IF (NCSIZE.GT.1) VMI = P_SUM(VMI)
!                 Check direction
                  IF (ELEV1 .GT. ELEV2) THEN
                      ELEVUS = ELEVUS + ELEV1
                      ELEVDS = ELEVDS + ELEV2
                  ELSE
                      ELEVUS = ELEVUS + ELEV2
                      ELEVDS = ELEVDS + ELEV1
                  ENDIF
                  UMEAN = UMEAN + UMI
                  VMEAN = VMEAN + VMI
                  COUNT = COUNT + 1.0D0              
              ENDIF
          ENDDO   
!         Setting a limiter here should hold everything
!         stable
          ELEVUS = MAX(ELEVUS / COUNT,0.01D0)
          ELEVDS = MAX(ELEVDS / COUNT,0.01D0)
          UMEAN = UMEAN / COUNT
          VMEAN = VMEAN / COUNT

!         Interpolate area upstream and downstream
!         Outside of bridge is received from HA Geo
!         Inside of the bridge is received from
!         HA bridge
!         compute area outside upstream AOUTUS
          DO J = 1, SIZE(HAGEO(I)%VEC) / 2 -1
              IF (ELEVUS .LE. 
     &               HAGEO(I)%VEC(1,1)) THEN
                  AOUTUS = 0.00001D0
              ELSEIF (ELEVUS .GE. 
     &      HAGEO(I)%VEC(1,SIZE(HAGEO(I)%VEC)/2)) THEN
              AOUTUS = HAGEO(I)%VEC(2,SIZE(HAGEO(I)%VEC)/2)
              ELSE
                  IF (ELEVUS.GT.HAGEO(I)%VEC(1,J).AND.
     &             ELEVUS.LE.HAGEO(I)%VEC(1,J+1)) THEN
                      AOUTUS = HAGEO(I)%VEC(2,J) + 
     &              (ELEVUS - HAGEO(I)%VEC(1,J))
     &             * ((HAGEO(I)%VEC(2,J+1)-HAGEO(I)%VEC(2,J))
     &             /(HAGEO(I)%VEC(1,J+1)-HAGEO(I)%VEC(1,J)))
                  ENDIF
              ENDIF
          ENDDO
!         compute area outside downstream AOUTDS
          DO J = 1, SIZE(HAGEO(I)%VEC) / 2 - 1
              IF (ELEVDS .LE. 
     &               HAGEO(I)%VEC(1,1)) THEN
                  AOUTDS = 0.00001D0
              ELSEIF (ELEVDS .GE. 
     &      HAGEO(I)%VEC(1,SIZE(HAGEO(I)%VEC)/2)) THEN
              AOUTDS = HAGEO(I)%VEC(2,SIZE(HAGEO(I)%VEC)/2)
              ELSE
                  IF (ELEVDS.GT.HAGEO(I)%VEC(1,J).AND.
     &             ELEVDS.LE.HAGEO(I)%VEC(1,J+1)) THEN
                      AOUTDS = HAGEO(I)%VEC(2,J) + 
     &              (ELEVDS - HAGEO(I)%VEC(1,J))
     &             * ((HAGEO(I)%VEC(2,J+1)-HAGEO(I)%VEC(2,J))
     &             /(HAGEO(I)%VEC(1,J+1)-HAGEO(I)%VEC(1,J)))
                  ENDIF
              ENDIF
          ENDDO
!         Compute area of bridge inside US AINUS
          DO J = 1, SIZE(HABRIDGE(I)%VEC) / 2 - 1
              IF (ELEVUS .LE. 
     &               HABRIDGE(I)%VEC(1,1)) THEN
                  ABRIUS = 0.00001D0
              ELSEIF (ELEVUS .GE. 
     &      HABRIDGE(I)%VEC(1,SIZE(HABRIDGE(I)%VEC)/2)) THEN
           ABRIUS = HABRIDGE(I)%VEC(2,SIZE(HABRIDGE(I)%VEC)/2)
              ELSE
                  IF (ELEVUS.GT.HABRIDGE(I)%VEC(1,J).AND.
     &             ELEVUS.LE.HABRIDGE(I)%VEC(1,J+1)) THEN
                      ABRIUS = HABRIDGE(I)%VEC(2,J) + 
     &          (ELEVUS - HABRIDGE(I)%VEC(1,J))
     &          * ((HABRIDGE(I)%VEC(2,J+1)-HABRIDGE(I)%VEC(2,J))
     &          /(HABRIDGE(I)%VEC(1,J+1)-HABRIDGE(I)%VEC(1,J)))
                  ENDIF
              ENDIF
          ENDDO
!         Compute area of bridge inside DS AINDS
          DO J = 1, SIZE(HABRIDGE(I)%VEC) / 2 - 1
              IF (ELEVDS .LE. 
     &               HABRIDGE(I)%VEC(1,1)) THEN
                  ABRIDS = 0.00001D0
              ELSEIF (ELEVDS .GE. 
     &      HABRIDGE(I)%VEC(1,SIZE(HABRIDGE(I)%VEC)/2)) THEN
           ABRIDS = HABRIDGE(I)%VEC(2,SIZE(HABRIDGE(I)%VEC)/2)
              ELSE
                  IF (ELEVDS.GT.HABRIDGE(I)%VEC(1,J).AND.
     &             ELEVDS.LE.HABRIDGE(I)%VEC(1,J+1)) THEN
                      ABRIDS = HABRIDGE(I)%VEC(2,J) + 
     &          (ELEVDS - HABRIDGE(I)%VEC(1,J))
     &          * ((HABRIDGE(I)%VEC(2,J+1)-HABRIDGE(I)%VEC(2,J))
     &          /(HABRIDGE(I)%VEC(1,J+1)-HABRIDGE(I)%VEC(1,J)))
                  ENDIF
              ENDIF
          ENDDO     
!         Compute flow areas inside
          AINDS = AOUTDS - ABRIDS
          AINUS = AOUTUS - ABRIDS
!         Compute average velocities v1 and v3
          V1 = FLXBRIDGE / AOUTUS
          V3 = FLXBRIDGE / AINDS
!         Check which method we are using to derive mean velocity
          IF (UMEANM .EQ. 2) THEN
              IF (NCSIZE .EQ. 1) THEN
                 V1 = SQRT(U%R(NODUMEANLOC(I))**2.D0 + 
     &               V%R(NODUMEANLOC(I))**2.D0)
                 V3 = V1 * (AOUTUS / AINDS)
              ELEVUS = ZF%R(NODUMEANLOC(I))+H%R(NODUMEANLOC(I))
              ELSE
                 IF (NODUMEANLOC(I) .NE. 0) THEN
                   V1 = SQRT(U%R(NODUMEANLOC(I))**2.D0 + 
     &               V%R(NODUMEANLOC(I))**2.D0)
                   V3 = V1 * (AOUTUS / AINDS)
                 ENDIF
                 IF (NCSIZE.GT.1) V1 = P_MAX(V1)
                 IF (NCSIZE.GT.1) V3 = P_MAX(V3)
              ENDIF
          ENDIF

!         Compute losses
!         BC contraction loss
!         MU according to weissbach
          MUWEI = 0.3D0 + 0.7D0 * (AINUS/AOUTUS)**3.D0
          print*, I, MUWEI
!          MUWEI = 0.5D0
          BLCO = V1**2.D0/2.D0/9.81D0*(1.D0/MUWEI-1.D0)**2.D0*
     &                (AOUTUS/AINUS)**2.D0
          BLEX = V3**2.D0/2.D0/9.81D0*(1.D0-AINDS/AOUTDS)**2.D0
!         Compute headloss maybe here we should add friction
!         at bridge deck as well
          DELTAH = BLCO + BLEX
!         Compute head loss gradient along interfaces
          allocate(GRADH(SIZE(NODESTRING(I)%VEC) / 2))
          GRADH(:) = 0
          allocate(GRADHX(SIZE(NODESTRING(I)%VEC) / 2))
          GRADHX(:) = 0
          allocate(GRADHY(SIZE(NODESTRING(I)%VEC) / 2))
          GRADHY(:) = 0
!         Distribute gradient
          DO J = 1, SIZE(NODESTRING(I)%VEC) / 2
              GRADH(J) = DELTAH / DISTTRI(I)%VEC(1,J)
          ENDDO
!         compute head loss gradients in x and y directions
          DO J = 1, SIZE(NODEBRI) - 1
              IF (NODEBRI(J) .NE. 0 .AND. 
     &              NODEBRI(J+1) .NE. 0) THEN
                  ANGLE = ATAN2(Y(NODEBRI(J+1))-Y(NODEBRI(J)),
     &      X(NODEBRI(J+1))- X(NODEBRI(J)))
                  ANGLE = ANGLE - 3.14159265D0/2D0
                  GRADHX(J) = GRADH(J) * COS(ANGLE)
                  GRADHY(J) = GRADH(J) * SIN(ANGLE)
              ENDIF
          ENDDO
!         Map head losses to nodes
          DO J = 1, SIZE(GRADHX)
              GNODX(J) = GNODX(J) + ABS(GRADHX(J) / 2.)
              GNODX(J+1) = GNODX(J+1) + ABS(GRADHX(J) / 2.)
              GNODY(J) = GNODY(J) + ABS(GRADHY(J) / 2.)
              GNODY(J+1) = GNODY(J+1) + ABS(GRADHY(J) / 2.)
          ENDDO
!         Apply headloss in the direction of the velocities
          DO J=1, SIZE(NODEBRI)
              IF (NODEBRI(J) .NE. 0) THEN
                  IF (H%R(NODEBRI(J)) .GT. 0.001D0) THEN
                   FU%R(NODEBRI(J)) = FU%R(NODEBRI(J))-9.81 * GNODX(J)*
     &        MAX(-1.0, MIN(1.0, UMEAN/abs(UMEAN)))
                   FV%R(NODEBRI(J)) = FV%R(NODEBRI(J))-9.81 * GNODY(J)*
     &        MAX(-1.0, MIN(1.0, VMEAN/abs(VMEAN)))
                  ENDIF
              ENDIF
          ENDDO
!         Deallocate helper arrays
	  deallocate(SEGQ)
          deallocate(SEGLENG)
          deallocate(NODEBRI)
          deallocate(GRADH)
          deallocate(GRADHX)
          deallocate(GRADHY)
          deallocate(GNODX)
          deallocate(GNODY)
      ENDDO
!
!
!=======================================================================
!
!     ICE PROCESSES
!
      IF(INCLUS(COUPLING,'KHIONE')) THEN
        IF(EQUA.NE.'SAINT-VENANT VF') THEN
          CALL SOURCE_HYDRO_ICOVER(NPOIN,FU,FV,H,U,V,T1,T2,T3,S,
     &      MESH,MSK,MASKEL,GRAV,KARMAN,CHESTR,DT,AT,KFROT,
     &      T4,T5,T6,CF,UNSV2D,0)
        ELSE
          CALL SOURCE_HYDRO_ICOVER(NPOIN,FU,FV,H,U,V,T1,T2,T3,S,
     &      MESH,MSK,MASKEL,GRAV,KARMAN,CHESTR,DT,AT+DT,KFROT,
     &      T4,T5,T6,CF,UNSV2D,1)
        ENDIF
      ENDIF
!
!=======================================================================
!
!  SECONDARY CURRENTS
!
      IF(SECCURRENTS) THEN
!
!       TAU_SEC
        DO K=1,NPOIN
          XX=H%R(K)*T%ADR(NTRAC)%P%R(K)
     &                        *SQRT(0.5D0*CF%R(K)*(U%R(K)**2+V%R(K)**2))
!         SEC_TAU IS USED ONLY FOR OUTPUTS
          SEC_TAU%R(K)=ROEAU*XX
!         ROEAU NOT CONSIDERED IN TAU_SEC, IT AVOIDS A DIVISION LATER
          T1%R(K) = XX*H%R(K)
        ENDDO
!       COMPUTING THE GRADIENTS OF H*TAU_SEC
!       WITH FACTOR V2DPAR=1/UNSV2D, REMOVED LATER
        CALL VECTOR(T2,'=','GRADF          X',IELMU,
     &              1.D0,T1,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(T3,'=','GRADF          Y',IELMU,
     &              1.D0,T1,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM (T2, 2, MESH)
          CALL PARCOM (T3, 2, MESH)
        ENDIF
!
        DO K=1,NPOIN
          IF(H%R(K).GE.1.D-4) THEN
            XX=1.D0/MAX(SQRT(U%R(K)**2+V%R(K)**2),1.D-12)
            T2%R(K)=UNSV2D%R(K)*(T2%R(K)*V%R(K)-T3%R(K)*U%R(K))*XX
            T7%R(K)=XX*(2.D0*T1%R(K)*SEC_R%R(K)+T2%R(K))/H%R(K)
!           FORCES ALONG X AND Y
            FU%R(K) = FU%R(K) - U%R(K)*T7%R(K)
            FV%R(K) = FV%R(K) - V%R(K)*T7%R(K)
          ENDIF
        ENDDO
!
      ENDIF
!
!=======================================================================
!
!  WIND
!
!                               1                         2     2
!              FU           =  --- * F    * U    * SQRT( U   + V    )
!                VENT           H     AIR    AIR          AIR   AIR
!
!
!                               1                         2     2
!              FV           =  --- * F    * V    * SQRT( U   + V    )
!                VENT           H     AIR    AIR          AIR   AIR
!
!
      IF(VENT) THEN
        YASMO=.TRUE.
!       TEMPORARY TREATMENT OF TIDAL FLATS
!       THE WIND EFFECT IS ONLY CONSIDERED IF THE WATER DEPTH IS
!       GREATER THAN 1 M.
!
        ROAIR = 1.3D0
        DO N=1,NPOIN
          IF (HN%R(N).GT.HWIND) THEN
            WD = SQRT( WINDX%R(N)**2 + WINDY%R(N)**2 )
            IF(FAIRACCU) THEN
!             A MORE ACCURATE TREATMENT
              IF(WD.LE.5.D0) THEN
                FAIR = ROAIR/ROEAU*0.565D-3
              ELSEIF (WD.LE.19.22D0) THEN
                FAIR = ROAIR/ROEAU*(-0.12D0+0.137D0*WD)*1.D-3
              ELSE
                FAIR = ROAIR/ROEAU*2.513D-3
              ENDIF
            ENDIF
            FU%R(N) = FU%R(N) + FAIR * WINDX%R(N) * WD / HN%R(N)
            FV%R(N) = FV%R(N) + FAIR * WINDY%R(N) * WD / HN%R(N)
          ENDIF
        ENDDO
!
      ENDIF
!
!=======================================================================
!
! CORIOLIS FORCE
!
!
!                FU           =  + FCOR * V
!                  CORIOLIS
!
!                FV           =  - FCOR * U
!                  CORIOLIS
!
      IF(CORIOL) THEN
!
        PI = 4.D0 * ATAN( 1.D0 )
!
        IF(SPHERI) THEN
!
          IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!         IN FINITE VOLUME THIS IS DONE IN SOURCE_MOMENT
!
            WROT = 2.D0 * PI / 86164.D0
            DO I=1,NPOIN
!             FORMULATION INDEPENDENT OF THE DIRECTION OF NORTH
              FU%R(I) = FU%R(I) + VN%R(I) * 2.D0 * WROT * SINLAT%R(I)
              FV%R(I) = FV%R(I) - UN%R(I) * 2.D0 * WROT * SINLAT%R(I)
            ENDDO
!
          ENDIF
!
!         TAKES THE TIDAL FORCE INTO ACCOUNT
!
          IF(MAREE) THEN
            CALL MARAST(MARDAT,MARTIM,PHI0,NPOIN,AT,
     &                  FU%R,FV%R,MESH%X%R,SINLAT%R,COSLAT%R,GRAV)
          ENDIF
!
          IF(LT.EQ.1) THEN
            WRITE(LU,12)
          ENDIF
12        FORMAT(1X,'PROSOU : IN SPHERICAL COORDINATES, THE CORIOLIS',/,
     &           1X,'         PARAMETER DEPENDS ON THE LATITUDE.',/,
     &           1X,'         THE KEY WORD ''CORIOLIS COEFFICIENT''',/,
     &           1X,'         IS CONSEQUENTLY IGNORED.')
!
        ELSE
          IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!         IN FINITE VOLUME THIS IS DONE IN SOURCE_MOMENT
!
            CALL OS( 'X=X+CY  ' , X=FU , Y=VN , C=FCOR )
            CALL OS( 'X=X+CY  ' , X=FV , Y=UN , C=-FCOR )
!
          ENDIF
!
          IF(LT.EQ.1) THEN
            WRITE(LU,22)
          ENDIF
22        FORMAT(1X,'PROSOU : IN CARTESIAN COORDINATES, THE CORIOLIS',/,
     &           1X,'         PARAMETER IS READ IN THE STEERING FILE',/,
     &           1X,'         IT IS THE KEY WORD ''CORIOLIS',/,
     &           1X,'         COEFFICIENT'', IT IS UNIFORM IN SPACE')
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  THE SECOND MEMBERS ARE PROPERLY DISCRETISED
!
      IELMU=UN%ELM
!
      IF(IELMU.NE.IELM1) THEN
        CALL CHGDIS(FU,IELM1,IELMU,MESH)
        CALL CHGDIS(FV,IELM1,IELMU,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL CPSTVC(HN,SMH)
      YASMH=.FALSE.
      CALL OS('X=0     ',X=SMH)
!
!     RAIN-EVAPORATION
!
      IF(RAIN) THEN
!
        IF(EQUA(1:15).EQ.'SAINT-VENANT VF') YASMH = .TRUE.
!       RAIN OR EVAPORATION = 0 FOR AT > RAIN_HDUR
!
        IF(AT.LE.RAIN_HDUR*3600.D0) THEN
          RAIN_MPS=RAIN_MMPD/86400000.D0
        ELSE
          RAIN_MPS=0.D0
        ENDIF
        SURDT=1.D0/DT
        IF(BANDEC) THEN
          IF(RUNOFFOPT.EQ.0) THEN
!           EVAPORATION (TENTATIVELY...) LIMITED BY AVAILABLE WATER
            DO I=1,NPOIN
              PLUIE%R(I)=MAX(RAIN_MPS,-MAX(HN%R(I),0.D0)*SURDT)
            ENDDO
          ELSEIF(RUNOFFOPT.EQ.1) THEN
            CALL RUNOFF_SCS_CN(PLUIE,T1%R,T2%R,T3%R,ACCROF,RAIN_MPS,AMC,
     &                         CN,ZF,ZFSLOP,RAIN_HDUR,T2D_FILES,T2DFO2,
     &                         NPOIN,MASKEL,MSK,IELM1,MESH,T8,T9,
     &                         T10,T11%R)
          ELSEIF(RUNOFFOPT.EQ.2) THEN
            CALL RUNOFF_HORTON(PLUIE,ACCINF,T3%R,ACCROF,RAIN_MPS,AMC,
     &                         FC,F0,RAIN_HDUR,T2D_FILES,T2DFO2,
     &                         NPOIN,MESH,T8%R,T9%R)
          ELSEIF(RUNOFFOPT.EQ.3) THEN
            CALL RUNOFF_GREENAMPT(PLUIE,ACCINF,T3%R,ACCROF,RAIN_MPS,
     &                            KS,RAIN_HDUR,T2D_FILES,T2DFO2,
     &                            NPOIN,MESH,T8%R,T9%R)
          ELSE
            WRITE(LU,222)
222         FORMAT(1X,'PROSOU : RUNOFF MODEL NOT IMPLEMENTED YET',/,
     &             1X,'         AVAILABLE OPTIONS ARE:',/,
     &             1X,'         0 : NO INFILTRATION',/,
     &             1X,'         1 : SCS CN MODEL',/,
     &             1X,'         2 : HORTON MODEL',/,
     &             1X,'         3 : GREEN-AMPT MODEL')
!
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSE
          IF(RUNOFFOPT.EQ.1.OR.RUNOFFOPT.EQ.2.OR.RUNOFFOPT.EQ.3) THEN
            WRITE(LU,224)
224         FORMAT(1X,'PROSOU : TIDAL FLATS OPTION MUST BE ACTIVATED',/,
     &             1X,'         WITH SCS CN, HORTON OR G-AMPT MODEL')
!
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL OS('X=C     ',X=PLUIE,C=RAIN_MPS)
        ENDIF
!
        CALL USER_RAIN
!
      ENDIF
!
      CALL USER_RAIN
!
!     SOURCES
!
      IF(NREJET.GT.0) THEN
!
        YASMH = .TRUE.
        YASMO = .TRUE.
!
!       SOURCE TERMS IN THE CONTINUITY EQUATION
!       BEWARE, SMH IS ALSO USED FOR TRACER
!
!       CASE OF SOURCES GIVEN BY POINTS (NOT BY REGIONS)
        IF(NREG.EQ.0) THEN
!
          DO I = 1 , NREJET
            IR = ISCE(I)
!           THE TEST IS USEFUL IN PARALLEL MODE, WHEN THE POINT SOURCE
!           IS NOT IN THE SUB-DOMAIN
            IF(IR.GT.0) THEN
              IF(OPTSOU.EQ.1) THEN
!               "NORMAL" VERSION
                SMH%R(IR)=SMH%R(IR)+DSCE(I)*UNSV2D%R(IR)
                IF(EQUA.EQ.'SAINT-VENANT VF') THEN
                  WRITE(LU,323)
                  CALL PLANTE(1)
                  STOP
                ENDIF
              ELSE
!               "DIRAC" VERSION
                IF(NCSIZE.GT.1) THEN
                  SMH%R(IR)=SMH%R(IR)+DSCE(I)*MESH%IFAC%I(IR)
                ELSE
                  SMH%R(IR)=SMH%R(IR)+DSCE(I)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
!
!         SOURCE TERMS IN THE MOMENTUM EQUATIONS
!         EXPLICIT TREATMENT OF MOMENTUM CONTRIBUTIONS TO THE SOURCES
!
          IF(NREJEU.GT.0) THEN
            DO I = 1 , NREJEU
              IR = ISCE(I)
!             THE TEST IS USEFUL IN PARALLEL MODE, WHEN THE POINT SOURCE
!             IS NOT IN THE SUB-DOMAIN
              IF(IR.GT.0) THEN
!               MOMENTUM ADDED BY THE SOURCE
!      -        MOMENTUM TAKEN BY THE SOURCE
                FU%R(IR)=FU%R(IR) + (VUSCE(AT,I)-UN%R(IR))*
     &          DSCE(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
                FV%R(IR)=FV%R(IR) + (VVSCE(AT,I)-VN%R(IR))*
     &          DSCE(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
              ENDIF
            ENDDO
          ENDIF
!
        ELSE !NREG.NE.0
!
          DO IREG=1, NREG
            TTL=TNP(IREG)
!           TEST USEFUL FOR PARALLEL MODE
            IF(TTL.NE.0) THEN
              DO I=1,TTL
                II=PT_IN_POLY(IREG,I)
                IF(OPTSOU.EQ.1) THEN
!                 "NORMAL" VERSION
                  SMH%R(II)=SMH%R(II)+DSCE(IREG)/AREA_P(IREG)
                  IF(EQUA.EQ.'SAINT-VENANT VF') THEN
                    WRITE(LU,323)
                    CALL PLANTE(1)
                    STOP
                  ENDIF
                ELSE
!                 "DIRAC" VERSION
                  IF(NCSIZE.GT.1) THEN
                    SMH%R(II)=SMH%R(II)+DSCE(IREG)*MESH%IFAC%I(II)
                  ELSE
                    SMH%R(II)=SMH%R(II)+DSCE(IREG)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
!
!         SOURCE TERMS IN THE MOMENTUM EQUATIONS
!         EXPLICIT TREATMENT OF MOMENTUM CONTRIBUTIONS TO THE SOURCES
!
          IF(NREJEU.GT.0) THEN
            DO IREG=1, NREG
              TTL=TNP(IREG)
              IF(TTL.NE.0) THEN
                DO I=1,TTL
                  II=PT_IN_POLY(IREG,I)
                  FU%R(II)=FU%R(II) + (VUSCE(AT,IREG)-UN%R(II))*
     &            DSCE(IREG)/AREA_P(IREG)/MAX(HN%R(II),0.1D0)
                  FV%R(II)=FV%R(II) + (VVSCE(AT,IREG)-VN%R(II))*
     &            DSCE(IREG)/AREA_P(IREG)/MAX(HN%R(II),0.1D0)
                ENDDO
              ENDIF
            ENDDO
          ENDIF
!
        ENDIF
      ENDIF
!
!     CULVERTS OR SIPHONS
!
      IF(NBUSE.GT.0) THEN
!
        YASMH = .TRUE.
        YASMO = .TRUE.
!
        DO I = 1 , NBUSE
          IR = ENTBUS(I)
          IF(IR.GT.0) THEN
            IF(OPTSOU.EQ.1) THEN
!             "NORMAL" VERSION
              SMH%R(IR)=SMH%R(IR)-DBUS(I)*UNSV2D%R(IR)
              IF(EQUA.EQ.'SAINT-VENANT VF') THEN
                WRITE(LU,323)
                CALL PLANTE(1)
                STOP
              ENDIF
            ELSE
!             "DIRAC" VERSION
              IF(NCSIZE.GT.1.AND.EQUA(1:15).NE.'SAINT-VENANT VF') THEN
                SMH%R(IR)=SMH%R(IR)-DBUS(I)*MESH%IFAC%I(IR)
              ELSE
                SMH%R(IR)=SMH%R(IR)-DBUS(I)
              ENDIF
            ENDIF
            FU%R(IR) = FU%R(IR) - (UBUS(1,I)-UN%R(IR))*
     &      DBUS(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
            FV%R(IR) = FV%R(IR) - (VBUS(1,I)-VN%R(IR))*
     &      DBUS(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
          ENDIF
          IR = SORBUS(I)
          IF(IR.GT.0) THEN
            IF(OPTSOU.EQ.1) THEN
!             "NORMAL" VERSION
              SMH%R(IR)=SMH%R(IR)+DBUS(I)*UNSV2D%R(IR)
              IF(EQUA.EQ.'SAINT-VENANT VF') THEN
                WRITE(LU,323)
                CALL PLANTE(1)
                STOP
              ENDIF
            ELSE
!             "DIRAC" VERSION
              IF(NCSIZE.GT.1.AND.EQUA(1:15).NE.'SAINT-VENANT VF') THEN
                SMH%R(IR)=SMH%R(IR)+DBUS(I)*MESH%IFAC%I(IR)
              ELSE
                SMH%R(IR)=SMH%R(IR)+DBUS(I)
              ENDIF
            ENDIF
            FU%R(IR) = FU%R(IR) + (UBUS(2,I)-UN%R(IR))*
     &      DBUS(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
            FV%R(IR) = FV%R(IR) + (VBUS(2,I)-VN%R(IR))*
     &      DBUS(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
          ENDIF
        ENDDO
      ENDIF
!
!     WEIRS (ONLY IF TYPSEUIL=2)
!
      IF(NWEIRS.GT.0.AND.TYPSEUIL.EQ.2) THEN
!
        YASMH = .TRUE.
        YASMO = .TRUE.
!
        DO N=1,N_NGHB_W_NODES
          IF(WNODES_PROC(N)%NUM_NEIGH.EQ.IPID) GOTO 50
        ENDDO
50      CONTINUE
        DO I=1, WNODES_PROC(N)%NB_NODES
          IR = WNODES_PROC(N)%NUM_LOC(I)
          K  = WNODES_PROC(N)%LIST_NODES(I)
          SMH%R(IR) = SMH%R(IR) + WNODES(K)%QN * UNSV2D%R(IR)
! QUANTITY OF MOVEMENTS NOT TAKEN INTO ACCOUNT FOR THE MOMENT
! The following lines generate instability and crash
! Probably because we would like to impose velocities accross  solid boundaries!
!
!         FU%R(IR) = FU%R(IR) + (UWEIRA%ADR(N)%P%R(I)-UN%R(IR))*
!     &      WNODES(K)%QN*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
!         FV%R(IR) = FV%R(IR) + (VWEIRA%ADR(N)%P%R(I)-VN%R(IR))*
!     &      WNODES(K)%QN*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
        ENDDO
      ENDIF
!
323   FORMAT(1X,'PROSOU: ONLY SOURCES WITH DIRAC OPTION ',/,
     &       1X,'           ARE IMPLEMENTED WITH FINITE VOLUMES  '  ,/,
     &       1X,'           PLEASE SET ',/,
     &       1X,'           "TYPE OF SOURCES = 2"           ')
!
!=======================================================================
!
!  WAVE DRIVEN CURRENTS
!
!
!                FU        =  FXWAVE
!                  COUROU
!
!                FV        =  FYWAVE
!                  COUROU
!
!       FXWAVE AND FYWAVE ARE TAKEN IN A RESULTS FILE FROM
!       ARTEMIS OR TOMAWAC
!
!       BEWARE   : 1. MESHES MUST BE THE SAME
!       ---------
!
!                  2. STATIONARY FORCING
!
      IF(COUROU) THEN
!
!       WITH NO COUPLING, TAKING THE WAVE STRESSES ONCE FOR ALL
!       IN A BINARY DATA FILE
!
        IF(.NOT.PROSOU_DEJALU.AND..NOT.INCLUS(COUPLING,'TOMAWAC')) THEN
!
            ! Records numbering starts from 0
            IREC = NPTH - 1
!           NBI1 : BINARY DATA FILE 1
            NOMX='FORCE FX        M/S2            '
            NOMY='FORCE FY        M/S2            '
            FFORMAT = T2D_FILES(T2DBI1)%FMT
            FILE_ID = T2D_FILES(T2DBI1)%LU
            CALL GET_MESH_NPOIN(FFORMAT, FILE_ID, TRIANGLE_ELT_TYPE,
     &                          NP, ERR)
            CALL CHECK_CALL(ERR,'PROSOU:GET_MESH_NPOIN')
            CALL FIND_VARIABLE(FFORMAT, FILE_ID, NOMX, FXWAVE%R, NPOIN,
     &                     ERR, RECORD=IREC, TIME_RECORD=ATH)
            OKX = ERR.EQ.0
            CALL FIND_VARIABLE(FFORMAT, FILE_ID, NOMY, FYWAVE%R, NPOIN,
     &                     ERR, RECORD=IREC, TIME_RECORD=ATH)
            OKY = ERR.EQ.0
            IF(.NOT.OKX.OR..NOT.OKY) THEN
!             SECOND TRY (OLD VERSIONS OF ARTEMIS OR TOMAWAC)
              NOMX='FORCE_FX                      '
              NOMY='FORCE_FY                      '
              CALL FIND_VARIABLE(FFORMAT, FILE_ID,NOMX, FXWAVE%R, NPOIN,
     &                       ERR, RECORD=IREC,TIME_RECORD=ATH)
              OKX = ERR.EQ.0
              CALL FIND_VARIABLE(FFORMAT, FILE_ID,NOMY, FYWAVE%R, NPOIN,
     &                       ERR, RECORD=IREC,TIME_RECORD=ATH)
              OKY = ERR.EQ.0
            ENDIF
!           CLANDESTINE VARIABLES FROM TOMAWAC TO SISYPHE
            IF(NVARCL.GT.0) THEN
              DO I=1,NVARCL
                CALL FIND_VARIABLE(FFORMAT, FILE_ID,
     &                         VARCLA(I)(1:16),VARCL%ADR(I)%P%R, NPOIN,
     &                          ERR,RECORD=IREC,TIME_RECORD=ATH)
                IF(ERR.NE.0) THEN
                  WRITE(LU,8) VARCLA(I)(1:16)
8               FORMAT(1X,'PROSOU : CLANDESTINE VARIABLE:',/,1X,A16,/,
     &                 1X,'         NOT FOUND',/,1X,
     &                    '         IN THE WAVE RESULTS FILE')
                CALL PLANTE(1)
                STOP
                ENDIF
              ENDDO
            ENDIF
!
          IF(.NOT.OKX.OR..NOT.OKY) THEN
            WRITE(LU,6)
6           FORMAT(1X,'PROSOU: FORCE FX OR FY NOT FOUND',/,1X,
     &                '         IN THE WAVE RESULTS FILE')
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(NP.NE.NPOIN) THEN
            WRITE(LU,96)
 96         FORMAT(1X,'PROSOU: WAVE DRIVEN CURRENTS MODELLING.',/,
     &             1X,'WAVE AND CURRENT MODELS MESHES ARE ',/,
     &             1X,'DIFFERENT : NOT POSSIBLE AT THE MOMENT.')
!
            CALL PLANTE(1)
            STOP
          ENDIF
!         WRITES OUT TO THE LISTING
          WRITE(LU,116) ATH
116       FORMAT(1X,/,1X,'PROSOU: WAVE DRIVEN CURRENTS MODELLING',/,
     &                1X,'         READING FILE AT TIME ',F10.3,/)
          IF(IELMU.NE.IELM1) THEN
            CALL CHGDIS(FXWAVE,IELM1,IELMU,MESH)
            CALL CHGDIS(FYWAVE,IELM1,IELMU,MESH)
          ENDIF
          PROSOU_DEJALU = .TRUE.
!
        ENDIF
!
!       ADDS INTO FU AND FV
!
        IF(INCLUS(COUPLING,'TOMAWAC')) THEN
          IF(IELMU.NE.IELM1) THEN
            CALL CHGDIS(FXWAVE,IELM1,IELMU,MESH)
            CALL CHGDIS(FYWAVE,IELM1,IELMU,MESH)
          ENDIF
        ENDIF
        CALL OS('X=X+Y   ',X=FU,Y=FXWAVE)
        CALL OS('X=X+Y   ',X=FV,Y=FYWAVE)
!
      ENDIF
!
!=======================================================================
!
!-----------------------------------------------------------------------
!
!     TAKES SEEPAGE IN THE SOIL INTO ACCOUNT
!     COMMUNICATES WITH ESTEL-3D
!
!     GETS SOURCE TERM FROM ESTEL-3D TO ACCOUNT FOR SEEPAGE
!     CALLS THE INFILTRATION ROUTINE
!
      CALL INFILTRATION_GET(SMH%R,UNSV2D%R,YASMH)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
