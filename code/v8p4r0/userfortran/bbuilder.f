!                       ******************
                        SUBROUTINE BBUILDER
!                       ******************
!
     & (NBRIDGES,IKLE)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!>@brief    Initializes Bridge structures
!
!history  S. Gegenleithner, C. Dorfmann (flow engineering)
!+        30/03/2023
!+        First working version
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param  [in]  NBRIDGES    NUMBER OF BRIDGES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL, ONLY : P_MIN, P_MAX, P_SUM
      USE BIEF_DEF, ONLY:NCSIZE
      USE INTERFACE_TELEMAC2D, EX_BBUILDER => BBUILDER
      USE DECLARATIONS_TELEMAC2D, ONLY: NPOIN, T2D_FILES,BRILOC,BRISEC, 
     &                 MESH, NELEM, X,Y, ZF,NELMAX,BRIDGE_BLOCK_LOC,
     &                 BRIDGE_BLOCK_SEC,BRIDGE_GEO_CS,NODELOCAL,
     &                 BRIDGE_BLOCK_LOC_RES,NODESTRING,HABRIDGE,HAGEO,
     &                 BNEIGHGLOB,BNEIGHLOC,DISTTRI,NPAIRLOC,NPAIRGLOB,
     &                 BRIDGE_BLOCK_SEC_RES,BRIDGE_BLOCK_GEO_RES,
     &                 HAGEOTEMP, HABRIDGETEMP, UMEANM, NODUMEAN,
     &                 NODUMEANLOC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: NBRIDGES
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,3)
!
!     Integer variables
      Integer I,J,K,L
      INTEGER :: INP, NVALS, ID, LENARRAY, NUMDIVS
      INTEGER :: GLOBCLOSE,STEPS,ADVANCED, COUNTER
      INTEGER :: I1, I2, I3,INDDEL,POSFILLED
      INTEGER :: LOC1,LOC2,CLOSEST,TEMPINT
      INTEGER :: I1G,I2G,I3G,FILLED,IND1,IND2,IND3
      INTEGER :: DISCRETY,DISCRETPOLY, DISCRETGEO
!     Logical variables
      LOGICAL :: ENDFOUND,CON2, CONTALL
!     String variables
      Character(10) :: FCHAR
!     Real variables
      REAL :: HELPER
!     Double precision variables
      DOUBLE PRECISION :: X1,Y1,X2,Y2, DIST,X3,Y3
      DOUBLE PRECISION :: ZINT,TOTLEN,MINY,XX1,YY1,CROSSP
      DOUBLE PRECISION :: RESSIZE,ALPH,BET,GAM,P1X,P2X,P3X,PX
      DOUBLE PRECISION :: P1Y,P2Y,P3Y,PY,ZF1,ZF2,ZF3, RATIO
      DOUBLE PRECISION :: MAXY, TEMPCPU,DISTI
      DOUBLE PRECISION :: MAXYB, MINYG, DIFF, STARTY, ENDY
      DOUBLE PRECISION :: XI1,XI2,XI3,YI1,YI2,YI3
      DOUBLE PRECISION :: HI,AREAI,AREABRIDGE, AREAGEO
      DOUBLE PRECISION :: FIRSTX, FIRSTY, LASTX, LASTY
!     Allocatable lists
      LOGICAL, DIMENSION(:), ALLOCATABLE :: MK3, MK4
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODELIST, UNILIST
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODETEMP, UNICLEAN
      INTEGER, DIMENSION(:), ALLOCATABLE :: DIVS
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DISTLIST
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DISTTEMP
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: CUMARR
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XTEMP
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: YTEMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     Write message for the bridge builder
      WRITE(LU,*) 'Initializing Bridge Structures'
!
!     Convert nodes
!
      IF (UMEANM .EQ. 2) THEN
          NODUMEANLOC(:) = 0
          DO I=1, NBRIDGES     
             NODUMEANLOC(I) =
     &   GLOBAL_TO_LOCAL_POINT(NODUMEAN(I),MESH)   
          ENDDO
      ENDIF
!
!-----------------------------------      
!         Read BK .i2s files
!-----------------------------------  
!
!-------- Read location file -------
!
!     Get the i2s location file from the TELEMAC files structure
      INP=T2D_FILES(BRILOC)%LU
!     Boolean that indicates if we found the end of the i2s header
      ENDFOUND = .FALSE.
!     Open loop for reading the file
      DO
!         Check if we already found the end of the header 
          IF (ENDFOUND .EQV. .TRUE.) THEN
!             Allocate the bridge block structures, with number of bridges
!             in domain
              allocate(BRIDGE_BLOCK_LOC(NBRIDGES))
!             Loop over all the bridges
              DO J=1, NBRIDGES
!                 Read the length of the bridge vector and the ID from the
!                 i2s file
                  READ (INP,*) NVALS, ID
!                 Allocate the bridge vectors
                  allocate(BRIDGE_BLOCK_LOC(J)%VEC(2,NVALS))
                  ! Loop over values
                  DO K=1, NVALS
!                     Read the actual coordinates of the i2s file
                      READ (INP,*) X1, Y1
!                     Write the values to the allocated vectors
                      BRIDGE_BLOCK_LOC(J)%VEC(1,K) = X1
                      BRIDGE_BLOCK_LOC(J)%VEC(2,K) = Y1
                  ENDDO
              ENDDO
!             Exit the loop after we finished processing the last bridge
              EXIT
!         If the end of the header was not found yet, continue checking
!         until found and set Boolean to .True.
          ELSE
              READ (INP,*) FCHAR
              IF (FCHAR == ':EndHeader' ) THEN
                  ENDFOUND = .TRUE.
              ENDIF
          ENDIF
      ENDDO
!
!-------- Read section file -------
!
!     Get the i2s section file from the TELEMAC files structure
      INP=T2D_FILES(BRISEC)%LU
!     Boolean that indicates if we found the end of the i2s header
      ENDFOUND = .FALSE.
!     Open loop for reading the file
      DO
!         Check if we already found the end of the header 
          IF (ENDFOUND .EQV. .TRUE.) THEN
!             Allocate the bridge block structures, with number of bridges
!             in domain
              allocate(BRIDGE_BLOCK_SEC(NBRIDGES))
!             Loop over all the bridges
              DO J=1, NBRIDGES
!                 Read the length of the bridge vector and the ID from the
!                 i2s file
                  READ (INP,*) NVALS, ID
!                 Allocate the bridge vectors
                  allocate(BRIDGE_BLOCK_SEC(J)%VEC(2,NVALS))
                  ! Loop over values
                  DO K=1, NVALS
!                     Read the actual coordinates of the i2s file
                      READ (INP,*) X1, Y1
!                     Write the values to the allocated vectors
                      BRIDGE_BLOCK_SEC(J)%VEC(1,K) = X1
                      BRIDGE_BLOCK_SEC(J)%VEC(2,K) = Y1
                  ENDDO
              ENDDO
!             Exit the loop after we finished processing the last bridge
              EXIT
!         If the end of the header was not found yet, continue checking
!         until found and set Boolean to .True.
          ELSE
              READ (INP,*) FCHAR
              IF (FCHAR == ':EndHeader' ) THEN
                  ENDFOUND = .TRUE.
              ENDIF
          ENDIF
      ENDDO
!
!-----------------------------------      
!    Derive stage discharge curve
!-----------------------------------    

!     Allocate bridge vectors to the number of total bridges
!     Allocate resampled structures
      allocate(BRIDGE_BLOCK_LOC_RES(NBRIDGES))
      allocate(BRIDGE_BLOCK_SEC_RES(NBRIDGES))
      allocate(BRIDGE_BLOCK_GEO_RES(NBRIDGES))
      allocate(BRIDGE_GEO_CS(NBRIDGES))
!     Allocate temporary functional relations
      allocate(HABRIDGETEMP(NBRIDGES))
      allocate(HAGEOTEMP(NBRIDGES))
!     Allocate final functional relations
      allocate(HABRIDGE(NBRIDGES))
      allocate(HAGEO(NBRIDGES))
!     Discretization for resampling
      DISCRETGEO = 500
      DISCRETPOLY = 1000
      DISCRETY = 1000
!     ---------------------------
!         Loop over bridges
!     ---------------------------
      DO I=1, NBRIDGES
!         ---------------------------
!             Resample crosssection
!         ---------------------------
          ! Determine number of divisions for resampling
          ! Compute length of the location polygon
          LENARRAY = SIZE(BRIDGE_BLOCK_SEC(I)%VEC) / 2
          TOTLEN = 0.
          DO J=1,LENARRAY - 1
              X1 = BRIDGE_BLOCK_SEC(I)%VEC(1,J)
              Y1 = BRIDGE_BLOCK_SEC(I)%VEC(2,J)
              X2 = BRIDGE_BLOCK_SEC(I)%VEC(1,J+1)
              Y2 = BRIDGE_BLOCK_SEC(I)%VEC(2,J+1)
!             Compute distance
              DIST = SQRT((X2-X1)**2D0 + (Y2-Y1)**2D0)
              TOTLEN = TOTLEN + DIST
          ENDDO
          ! Compute ressize for resampling
          RESSIZE = TOTLEN / DISCRETPOLY
          ! resample bridge geometry
          ! get number of divisions vector
          allocate(DIVS(SIZE(BRIDGE_BLOCK_SEC(I)%VEC)/2-1))
          DIVS(:) = 0
          ! For each segment init a number of divisions vector
          DO J=1,SIZE(BRIDGE_BLOCK_SEC(I)%VEC)/2-1
              X1 = BRIDGE_BLOCK_SEC(I)%VEC(1,J)
              Y1 = BRIDGE_BLOCK_SEC(I)%VEC(2,J)
              X2 = BRIDGE_BLOCK_SEC(I)%VEC(1,J+1)
              Y2 = BRIDGE_BLOCK_SEC(I)%VEC(2,J+1)
!             Compute distance
              DIST = SQRT((X2-X1)**2D0 + (Y2-Y1)**2D0)
!             Update Number of divisions array
              DIVS(J) = MAX(DIST / RESSIZE,1D0)
          ENDDO
          !         Resample crosssection
!         Determine number of the divisions
          NUMDIVS = SUM(DIVS) + LENARRAY
!         Allocate resampled bridge and write cornors
          allocate(BRIDGE_BLOCK_SEC_RES(I)%VEC(2,NUMDIVS))
          BRIDGE_BLOCK_SEC_RES(I)%VEC(:,:) = 0D0
!         write first entry to resampled array
          BRIDGE_BLOCK_SEC_RES(I)%VEC(1,1)
     &                         = BRIDGE_BLOCK_SEC(I)%VEC(1,1)
          BRIDGE_BLOCK_SEC_RES(I)%VEC(2,1)
     &                         = BRIDGE_BLOCK_SEC(I)%VEC(2,1)
          ADVANCED = 2
!         resample
          DO J=1,LENARRAY - 1
!             Get the two original coordinates that bound the seg
              X1 = BRIDGE_BLOCK_SEC(I)%VEC(1,J)
              Y1 = BRIDGE_BLOCK_SEC(I)%VEC(2,J)
              X2 = BRIDGE_BLOCK_SEC(I)%VEC(1,J+1)
              Y2 = BRIDGE_BLOCK_SEC(I)%VEC(2,J+1)
!             Loop over the number of divisions
              DO K=1, DIVS(J) + 1
!                 Compute X3_i and Y3_i which are equidistant
!                 distributed coordinates within the segment
                  X3 = X1 + K * (X2-X1)/(DIVS(J)+1)
                  Y3 = Y1 + K * (Y2-Y1)/(DIVS(J)+1)
!                 Write the coordinates to the resampled vector
                  BRIDGE_BLOCK_SEC_RES(I)%VEC(1,
     &                        ADVANCED) = X3
                  BRIDGE_BLOCK_SEC_RES(I)%VEC(2,
     &                        ADVANCED) = Y3 
!                 Advance advanced variable
                  ADVANCED = ADVANCED + 1
              ENDDO
           ENDDO
           deallocate(DIVS)
!         ---------------------------
!             Resample location
!         --------------------------- 
!         First resample the CS and extract coords
          LENARRAY = SIZE(BRIDGE_BLOCK_LOC(I)%VEC) / 2
          TOTLEN = 0.
          DO J=1,LENARRAY - 1
              X1 = BRIDGE_BLOCK_LOC(I)%VEC(1,J)
              Y1 = BRIDGE_BLOCK_LOC(I)%VEC(2,J)
              X2 = BRIDGE_BLOCK_LOC(I)%VEC(1,J+1)
              Y2 = BRIDGE_BLOCK_LOC(I)%VEC(2,J+1)
!             Compute distance
              DIST = SQRT((X2-X1)**2D0 + (Y2-Y1)**2D0)
              TOTLEN = TOTLEN + DIST
          ENDDO
          ! Compute ressize for resampling
          RESSIZE = TOTLEN / DISCRETGEO
          ! resample bridge geometry
          ! get number of divisions vector
          allocate(DIVS(SIZE(BRIDGE_BLOCK_LOC(I)%VEC)/2-1))
          DIVS(:) = 0
          ! For each segment init a number of divisions vector
          DO J=1,SIZE(BRIDGE_BLOCK_LOC(I)%VEC)/2-1
              X1 = BRIDGE_BLOCK_LOC(I)%VEC(1,J)
              Y1 = BRIDGE_BLOCK_LOC(I)%VEC(2,J)
              X2 = BRIDGE_BLOCK_LOC(I)%VEC(1,J+1)
              Y2 = BRIDGE_BLOCK_LOC(I)%VEC(2,J+1)
!             Compute distance
              DIST = SQRT((X2-X1)**2D0 + (Y2-Y1)**2D0)
!             Update Number of divisions array
              DIVS(J) = MAX(DIST / RESSIZE,1D0)
          ENDDO          
!         Resample location
!         Determine number of the divisions
          NUMDIVS = SUM(DIVS) + LENARRAY
!         Allocate resampled bridge and write cornors
          allocate(BRIDGE_BLOCK_LOC_RES(I)%VEC(2,NUMDIVS))
          BRIDGE_BLOCK_LOC_RES(I)%VEC(:,:) = 0D0
          BRIDGE_BLOCK_LOC_RES(I)%VEC(1,1)
     &                         = BRIDGE_BLOCK_LOC(I)%VEC(1,1)
          BRIDGE_BLOCK_LOC_RES(I)%VEC(2,1)
     &                         = BRIDGE_BLOCK_LOC(I)%VEC(2,1)
          allocate(BRIDGE_GEO_CS(I)%VEC(2,NUMDIVS + 3))
          BRIDGE_GEO_CS(I)%VEC(:,:) = 0.D0
          ADVANCED = 2
!         resample
          DO J=1,LENARRAY - 1
!             Get the two original coordinates that bound the seg
              X1 = BRIDGE_BLOCK_LOC(I)%VEC(1,J)
              Y1 = BRIDGE_BLOCK_LOC(I)%VEC(2,J)
              X2 = BRIDGE_BLOCK_LOC(I)%VEC(1,J+1)
              Y2 = BRIDGE_BLOCK_LOC(I)%VEC(2,J+1)
!             Loop over the number of divisions
              DO K=1, DIVS(J) + 1
!                 Compute X3_i and Y3_i which are equidistant
!                 distributed coordinates within the segment
                  X3 = X1 + K * (X2-X1)/(DIVS(J)+1)
                  Y3 = Y1 + K * (Y2-Y1)/(DIVS(J)+1)
!                 Write the coordinates to the resampled vector
                  BRIDGE_BLOCK_LOC_RES(I)%VEC(1,
     &                        ADVANCED) = X3
                  BRIDGE_BLOCK_LOC_RES(I)%VEC(2,
     &                        ADVANCED) = Y3 
!                 Advance advanced variable
                  ADVANCED = ADVANCED + 1
              ENDDO
          ENDDO
!         Compute cumulative array for section
          allocate(CUMARR(SIZE(BRIDGE_BLOCK_LOC_RES(I)%VEC)/2))
          CUMARR(:) = 0.0
          CUMARR(1) = 0.0
          DO J=1,SIZE(BRIDGE_BLOCK_LOC_RES(I)%VEC)/2 - 1
!             Get the two original coordinates that bound the seg
              X1 = BRIDGE_BLOCK_LOC_RES(I)%VEC(1,J)
              Y1 = BRIDGE_BLOCK_LOC_RES(I)%VEC(2,J)
              X2 = BRIDGE_BLOCK_LOC_RES(I)%VEC(1,J+1)
              Y2 = BRIDGE_BLOCK_LOC_RES(I)%VEC(2,J+1)
!             compute distance between segments
              DIST = SQRT((X2-X1)**2D0 + (Y2-Y1)**2D0)
!             write to array
              CUMARR(J+1) = CUMARR(J) + DIST
          ENDDO
!          Get cross section from TELEMAC Mesh
!          Loop over all points in the polyline
           DO J = 1,SIZE(BRIDGE_BLOCK_LOC_RES(I)%VEC)/2
!              retrieve point locations
               PX = BRIDGE_BLOCK_LOC_RES(I)%VEC(1,J)
               PY = BRIDGE_BLOCK_LOC_RES(I)%VEC(2,J)
!              check in which element the point is located
               DO K = 1,NELMAX
!                  Get indices from connectivity matrix
                   I1 = IKLE(K,1)
                   I2 = IKLE(K,2)
                   I3 = IKLE(K,3)
!                  Calculate barycentric coordinates
!                  Element coordinates
                   P1X = X(I1)
                   P2X = X(I2)
                   P3X = X(I3)
                   P1Y = Y(I1)
                   P2Y = Y(I2)
                   P3Y = Y(I3)
!                  compute alpha beta and gamma
                   ALPH = ((P2Y-P3Y)*(PX-P3X)+(P3X-P2X)*(PY-P3Y)) /
     &                    ((P2Y-P3Y)*(P1X-P3X)+(P3X-P2X)*(P1Y-P3Y))
                   BET = ((P3Y-P1Y)*(PX-P3X)+(P1X-P3X)*(PY-P3Y)) /
     &                    ((P2Y-P3Y)*(P1X-P3X)+(P3X-P2X)*(P1Y-P3Y))
                   GAM = 1.0 - ALPH - BET
!                  if all alpha beta and gamma are above zero
!                  The point lies within the triangle
                   IF (ALPH .GE. 0.0) THEN
                       IF (BET .GE. 0.0) THEN
                           IF (GAM .GE. 0.0) THEN
!                              Interpolate bathymetry
!                              Get Batymetrie coordinates at mesh
!                              nodes, we can use the bathycentric
!                              coordinates
                               ZF1 = ZF%R(I1)
                               ZF2 = ZF%R(I2)
                               ZF3 = ZF%R(I3)
!                              Interpolate
                               ZINT = ALPH*ZF1+BET*ZF2+GAM*ZF3
!                              add location from cum distance as x
                               BRIDGE_GEO_CS(I)%VEC(1,J+1) = CUMARR(J)
!                              and interpolated value as y
                               BRIDGE_GEO_CS(I)%VEC(2,J+1) = ZINT
                           ENDIF
                       ENDIF
                   ENDIF
               ENDDO
           ENDDO
!          Merge from processors
           DO J = 1, SIZE(BRIDGE_GEO_CS(I)%VEC) / 2
               TEMPCPU = BRIDGE_GEO_CS(I)%VEC(1,J)
               IF(NCSIZE.GT.1) BRIDGE_GEO_CS(I)%VEC(1,J)=P_MAX(TEMPCPU)
               TEMPCPU = BRIDGE_GEO_CS(I)%VEC(2,J)
               IF(NCSIZE.GT.1) BRIDGE_GEO_CS(I)%VEC(2,J)=P_MAX(TEMPCPU)
           ENDDO
!          Now we atificially highten corner points and close the poly
!          highten first point of polygon
           MAXYB = MAXVAL(BRIDGE_BLOCK_SEC(I)%VEC(2,:))
           MINYG = MINVAL(BRIDGE_GEO_CS(I)%VEC(2,2:
     &                SIZE(BRIDGE_GEO_CS(I)%VEC)/2 - 2))
           DIFF = ABS(MAXYB - MINYG) * 10D0
!          set remaining entries
!          first entry
           BRIDGE_GEO_CS(I)%VEC(1,1) = BRIDGE_GEO_CS(I)%VEC(1,2)
           BRIDGE_GEO_CS(I)%VEC(2,1) = BRIDGE_GEO_CS(I)%VEC(2,2)+DIFF
!          second to last
           BRIDGE_GEO_CS(I)%VEC(1,SIZE(BRIDGE_GEO_CS(I)%VEC)/2-1) = 
     &           BRIDGE_GEO_CS(I)%VEC(1,SIZE(BRIDGE_GEO_CS(I)%VEC)/2-2)
           BRIDGE_GEO_CS(I)%VEC(2,SIZE(BRIDGE_GEO_CS(I)%VEC)/2-1) = 
     &           BRIDGE_GEO_CS(I)%VEC(2,1)
!          last entry
           BRIDGE_GEO_CS(I)%VEC(1,SIZE(BRIDGE_GEO_CS(I)%VEC)/2) =
     &              BRIDGE_GEO_CS(I)%VEC(1,1)
           BRIDGE_GEO_CS(I)%VEC(2,SIZE(BRIDGE_GEO_CS(I)%VEC)/2) = 
     &              BRIDGE_GEO_CS(I)%VEC(2,1)
!          Resample closed geo polygon
           deallocate(DIVS)
           deallocate(CUMARR)
!         ---------------------------
!            Resample Crosssection geometry
!         --------------------------- 
          ! Determine number of divisions for resampling
          ! Compute length of the location polygon
          LENARRAY = SIZE(BRIDGE_GEO_CS(I)%VEC) / 2
          TOTLEN = 0.
          DO J=1,LENARRAY - 1
              X1 = BRIDGE_GEO_CS(I)%VEC(1,J)
              Y1 = BRIDGE_GEO_CS(I)%VEC(2,J)
              X2 = BRIDGE_GEO_CS(I)%VEC(1,J+1)
              Y2 = BRIDGE_GEO_CS(I)%VEC(2,J+1)
!             Compute distance
              DIST = SQRT((X2-X1)**2D0 + (Y2-Y1)**2D0)
              TOTLEN = TOTLEN + DIST
          ENDDO
          ! Compute ressize for resampling
          RESSIZE = TOTLEN / DISCRETPOLY
          ! resample bridge geometry
          ! get number of divisions vector
          allocate(DIVS(SIZE(BRIDGE_GEO_CS(I)%VEC)/2-1))
          DIVS(:) = 0
          ! For each segment init a number of divisions vector
          DO J=1,SIZE(BRIDGE_GEO_CS(I)%VEC)/2-1
              X1 = BRIDGE_GEO_CS(I)%VEC(1,J)
              Y1 = BRIDGE_GEO_CS(I)%VEC(2,J)
              X2 = BRIDGE_GEO_CS(I)%VEC(1,J+1)
              Y2 = BRIDGE_GEO_CS(I)%VEC(2,J+1)
!             Compute distance
              DIST = SQRT((X2-X1)**2D0 + (Y2-Y1)**2D0)
!             Update Number of divisions array
              DIVS(J) = MAX(DIST / RESSIZE,1D0)
          ENDDO
!         Resample crosssection
!         Determine number of the divisions
          NUMDIVS = SUM(DIVS) + LENARRAY
!         Allocate resampled bridge and write cornors
          allocate(BRIDGE_BLOCK_GEO_RES(I)%VEC(2,NUMDIVS))
          BRIDGE_BLOCK_GEO_RES(I)%VEC(:,:) = 0.D0
          BRIDGE_BLOCK_GEO_RES(I)%VEC(1,1)
     &                         = BRIDGE_GEO_CS(I)%VEC(1,1)
          BRIDGE_BLOCK_GEO_RES(I)%VEC(2,1)
     &                         = BRIDGE_GEO_CS(I)%VEC(2,1)
          ADVANCED = 2
!         resample
          DO J=1,LENARRAY - 1
!             Get the two original coordinates that bound the seg
              X1 = BRIDGE_GEO_CS(I)%VEC(1,J)
              Y1 = BRIDGE_GEO_CS(I)%VEC(2,J)
              X2 = BRIDGE_GEO_CS(I)%VEC(1,J+1)
              Y2 = BRIDGE_GEO_CS(I)%VEC(2,J+1)
!             Loop over the number of divisions
              DO K=1, DIVS(J) + 1
!                 Compute X3_i and Y3_i which are equidistant
!                 distributed coordinates within the segment
                  X3 = X1 + K * (X2-X1)/(DIVS(J)+1)
                  Y3 = Y1 + K * (Y2-Y1)/(DIVS(J)+1)
!                 Write the coordinates to the resampled vector
                  BRIDGE_BLOCK_GEO_RES(I)%VEC(1,
     &                        ADVANCED) = X3
                  BRIDGE_BLOCK_GEO_RES(I)%VEC(2,
     &                        ADVANCED) = Y3 
!                 Advance advanced variable
                  ADVANCED = ADVANCED + 1
              ENDDO
           ENDDO
           deallocate(DIVS)
      ENDDO
!     ---------------------------
!         Loop over bridges
!     ---------------------------
      DO I=1, NBRIDGES
!         Find bounds to compute area
          STARTY = MINVAL(BRIDGE_BLOCK_GEO_RES(I)%VEC(2,:))
          ENDY = MAXVAL(BRIDGE_BLOCK_GEO_RES(I)%VEC(2,:))
!         ---------------------------
!            Compute area of bridge
!         --------------------------- 
!         Find number of divisions and allocate
!         Based on bridge
          MINY = MINVAL(BRIDGE_BLOCK_SEC_RES(I)%VEC(2,:))
          MAXY = MAXVAL(BRIDGE_BLOCK_SEC_RES(I)%VEC(2,:))
          RESSIZE = (MAXY-MINY) / DISCRETY
!         Compute number of steps to take
          STEPS = (ENDY - STARTY) / RESSIZE
!         Loop over steps
          HI = STARTY  - 3 * RESSIZE
!         allocate bridge geo array
          allocate(HABRIDGETEMP(I)%VEC(2,STEPS))
          HABRIDGETEMP(I)%VEC(:,:) = 0D0
          allocate(XTEMP(SIZE(BRIDGE_BLOCK_SEC_RES(I)%VEC)/2))
          allocate(YTEMP(SIZE(BRIDGE_BLOCK_SEC_RES(I)%VEC)/2))
          XTEMP(:) = -9999
          YTEMP(:) = -9999
          DO J=1, STEPS
!             Find number of points below threshold
              COUNTER = 0
              DO K=1,SIZE(BRIDGE_BLOCK_SEC_RES(I)%VEC)/2
                  IF (BRIDGE_BLOCK_SEC_RES(I)%VEC(2,K).LE.HI) THEN
                      COUNTER = COUNTER + 1
                  ENDIF
              ENDDO
!             Fill arrays
              FILLED = 1
              DO K=1,SIZE(BRIDGE_BLOCK_SEC_RES(I)%VEC)/2
                  IF (BRIDGE_BLOCK_SEC_RES(I)%VEC(2,K).LE.HI) THEN
                      XTEMP(FILLED) = BRIDGE_BLOCK_SEC_RES(I)%VEC(1,K)
                      YTEMP(FILLED) = BRIDGE_BLOCK_SEC_RES(I)%VEC(2,K)
                      FILLED = FILLED + 1
                  ENDIF
              ENDDO
!             Compute area
              FIRSTX = XTEMP(1)
              FIRSTY = YTEMP(1)
              AREABRIDGE = 0.D0
              DO K=1, SIZE(XTEMP) - 1
                  X1 = XTEMP(K)
                  X2 = XTEMP(K+1)
                  Y1 = YTEMP(K)
                  Y2 = YTEMP(K+1)
!                 end found
                  IF (X2.LT.-1000) THEN
                      AREAI = X1*FIRSTY-Y1*FIRSTX
                      AREABRIDGE = AREABRIDGE + AREAI
                      EXIT
                  ELSEIF (K .EQ. SIZE(XTEMP)-1) THEN
                      AREAI = X2*FIRSTY-Y2*FIRSTX
                      AREABRIDGE = AREABRIDGE + AREAI
                      EXIT
                  ELSE
                      AREAI = X1 * Y2 - Y1 * X2
                      AREABRIDGE = AREABRIDGE + AREAI
                  ENDIF
              ENDDO
              AREABRIDGE = ABS(AREABRIDGE) / 2.D0
              XTEMP(:) = -9999
              YTEMP(:) = -9999
              HI = HI + RESSIZE
              HABRIDGETEMP(I)%VEC(1,J) = HI
              HABRIDGETEMP(I)%VEC(2,J) = AREABRIDGE
          ENDDO
          deallocate(XTEMP)
          deallocate(YTEMP)
!         ---------------------------
!            Compute area geo
!         --------------------------- 
!         Find number of divisions and allocate
!         Based on bridge
          MINY = MINVAL(BRIDGE_BLOCK_GEO_RES(I)%VEC(2,:))
          MAXY = MAXVAL(BRIDGE_BLOCK_GEO_RES(I)%VEC(2,:))
          RESSIZE = (MAXY-MINY) / DISCRETY
!         Compute number of steps to take
          STEPS = (ENDY - STARTY) / RESSIZE
!         Loop over steps
          HI = STARTY - 3 * RESSIZE
!         allocate bridge geo array
          allocate(HAGEOTEMP(I)%VEC(2,STEPS))
          HAGEOTEMP(I)%VEC(:,:) = 0D0
          allocate(XTEMP(SIZE(BRIDGE_BLOCK_GEO_RES(I)%VEC)/2))
          allocate(YTEMP(SIZE(BRIDGE_BLOCK_GEO_RES(I)%VEC)/2))
          XTEMP(:) = -9999
          YTEMP(:) = -9999
          DO J=1, STEPS
!             Find number of points below threshold
              COUNTER = 0
              DO K=1,SIZE(BRIDGE_BLOCK_GEO_RES(I)%VEC)/2
                  IF (BRIDGE_BLOCK_GEO_RES(I)%VEC(2,K).LE.HI) THEN
                      COUNTER = COUNTER + 1
                  ENDIF
              ENDDO
!             Fill arrays
              FILLED = 1
              DO K=1,SIZE(BRIDGE_BLOCK_GEO_RES(I)%VEC)/2
                  IF (BRIDGE_BLOCK_GEO_RES(I)%VEC(2,K).LE.HI) THEN
                      XTEMP(FILLED) = BRIDGE_BLOCK_GEO_RES(I)%VEC(1,K)
                      YTEMP(FILLED) = BRIDGE_BLOCK_GEO_RES(I)%VEC(2,K)
                      FILLED = FILLED + 1
                  ENDIF
              ENDDO
!             Compute area
              FIRSTX = XTEMP(1)
              FIRSTY = YTEMP(1)
              AREABRIDGE = 0.D0
              DO K=1, SIZE(XTEMP) - 1
                  X1 = XTEMP(K)
                  X2 = XTEMP(K+1)
                  Y1 = YTEMP(K)
                  Y2 = YTEMP(K+1)
!                 end found
                  IF (X2.LT.-1000) THEN
                      AREAI = X1*FIRSTY-Y1*FIRSTX
                      AREABRIDGE = AREABRIDGE + AREAI
                      EXIT
                  ELSEIF (K .EQ. SIZE(XTEMP)-1) THEN
                      AREAI = X2*FIRSTY-Y2*FIRSTX
                      AREABRIDGE = AREABRIDGE + AREAI
                      EXIT
                  ELSE
                      AREAI = X1 * Y2 - Y1 * X2
                      AREABRIDGE = AREABRIDGE + AREAI
                  ENDIF
              ENDDO
              AREABRIDGE = ABS(AREABRIDGE) / 2.D0
              XTEMP(:) = -9999
              YTEMP(:) = -9999
              HI = HI + RESSIZE
              HAGEOTEMP(I)%VEC(1,J) = HI
              HAGEOTEMP(I)%VEC(2,J) = AREABRIDGE
          ENDDO
          deallocate(XTEMP)
          deallocate(YTEMP)
      ENDDO
!     Condense geometry by linearization and ratio
!     ---------------------------
!         Loop over bridges
!     ---------------------------
!     Set ratio
      RATIO = 1.02
      DO I=1, NBRIDGES
!         ---------------------------
!            Condense bridge geometry
!         ---------------------------
!         start at two to inlude at least first and last point
          COUNTER = 2
!         Last found coordinates
          X1 = -9999D0
          Y1 = -9999D0
          DO J = 1, SIZE(HABRIDGETEMP(I)%VEC) / 2 - 1
              ! Current coordinates
              X2 = HABRIDGETEMP(I)%VEC(1,J)
              Y2 = HABRIDGETEMP(I)%VEC(2,J)
              DIFF = ABS(Y2 / MAX(Y1,0.00001D0))
              IF (DIFF .GE. RATIO) THEN
                  Y1 = Y2
                  COUNTER = COUNTER + 1
              ENDIF
          ENDDO
!         allocate final geo
          allocate(HABRIDGE(I)%VEC(2,COUNTER))
!         add firts and last point
          HABRIDGE(I)%VEC(1,1) = HABRIDGETEMP(I)%VEC(1,1)
          HABRIDGE(I)%VEC(2,1) = HABRIDGETEMP(I)%VEC(2,1)
          HABRIDGE(I)%VEC(1,SIZE(HABRIDGE(I)%VEC)/2) = 
     &        HABRIDGETEMP(I)%VEC(1,SIZE(HABRIDGETEMP(I)%VEC)/2)
          HABRIDGE(I)%VEC(2,SIZE(HABRIDGE(I)%VEC)/2) = 
     &        HABRIDGETEMP(I)%VEC(2,SIZE(HABRIDGETEMP(I)%VEC)/2)
!         Fill values
          COUNTER = 1
!         Last found coordinates
          X1 = -9999D0
          Y1 = -9999D0
          DO J = 1, SIZE(HABRIDGETEMP(I)%VEC) / 2 - 1
              ! Current coordinates
              X2 = HABRIDGETEMP(I)%VEC(1,J)
              Y2 = HABRIDGETEMP(I)%VEC(2,J)
              DIFF = ABS(Y2 / MAX(Y1,0.00001D0))
              IF (DIFF .GE. RATIO) THEN
                  Y1 = Y2
                  COUNTER = COUNTER + 1
                  HABRIDGE(I)%VEC(1,COUNTER) = X2
                  HABRIDGE(I)%VEC(2,COUNTER) = Y2
              ENDIF
          ENDDO
!         ---------------------------
!            Condense geo geometry
!         ---------------------------
!         start at two to inlude at least first and last point
          COUNTER = 2
!         Last found coordinates
          X1 = -9999D0
          Y1 = -9999D0
          DO J = 1, SIZE(HAGEOTEMP(I)%VEC) / 2 - 1
              ! Current coordinates
              X2 = HAGEOTEMP(I)%VEC(1,J)
              Y2 = HAGEOTEMP(I)%VEC(2,J)
              DIFF = ABS(Y2 / MAX(Y1,0.00001D0))
              IF (DIFF .GE. RATIO) THEN
                  Y1 = Y2
                  COUNTER = COUNTER + 1
              ENDIF
          ENDDO
!         allocate final geo
          allocate(HAGEO(I)%VEC(2,COUNTER))
!         add firts and last point
          HAGEO(I)%VEC(1,1) = HAGEOTEMP(I)%VEC(1,1)
          HAGEO(I)%VEC(2,1) = HAGEOTEMP(I)%VEC(2,1)
          HAGEO(I)%VEC(1,SIZE(HAGEO(I)%VEC)/2) = 
     &        HAGEOTEMP(I)%VEC(1,SIZE(HAGEOTEMP(I)%VEC)/2)
          HAGEO(I)%VEC(2,SIZE(HAGEO(I)%VEC)/2) = 
     &        HAGEOTEMP(I)%VEC(2,SIZE(HAGEOTEMP(I)%VEC)/2)
!         Fill values
          COUNTER = 1
!         Last found coordinates
          X1 = -9999D0
          Y1 = -9999D0
          DO J = 1, SIZE(HAGEOTEMP(I)%VEC) / 2 - 1
              ! Current coordinates
              X2 = HAGEOTEMP(I)%VEC(1,J)
              Y2 = HAGEOTEMP(I)%VEC(2,J)
              DIFF = ABS(Y2 / MAX(Y1,0.00001D0))
              IF (DIFF .GE. RATIO) THEN
                  Y1 = Y2
                  COUNTER = COUNTER + 1
                  HAGEO(I)%VEC(1,COUNTER) = X2
                  HAGEO(I)%VEC(2,COUNTER) = Y2
              ENDIF
          ENDDO
      ENDDO
!---- Build node connectivity  ----      
!     Find nodestrings for discharge computation
!     we use global node numbers
!     Nodestings
      allocate(NODESTRING(NBRIDGES))
      allocate(NODELOCAL(NBRIDGES))
!     Allocate bridge neighbors
      allocate(BNEIGHGLOB(NBRIDGES))
      allocate(BNEIGHLOC(NBRIDGES))
!     Allocate distances between triangles
      allocate(DISTTRI(NBRIDGES))
!     Allocate node pairs
      allocate(NPAIRLOC(NBRIDGES))
      allocate(NPAIRGLOB(NBRIDGES))
!     Loop over all bridges again
      DO I=1, NBRIDGES
!         allocate temp nodestings
          FILLED = 1
!         This is very slow, if it gets too slow
!         use k-d tree or similar stuff
!         allocate node list
          allocate(NODELIST(SIZE(BRIDGE_BLOCK_LOC_RES(I)%VEC)/2))
          allocate(NODETEMP(SIZE(NODELIST)))
!         allocate boolean 
          allocate(MK3(SIZE(NODELIST)))
          allocate(DISTLIST(SIZE(NODELIST)))
          allocate(DISTTEMP(SIZE(NODELIST)))
          MK3(:) = .TRUE.
          NODELIST(:) = 0
          GLOBCLOSE = 0
          DO J = 1,SIZE(BRIDGE_BLOCK_LOC_RES(I)%VEC)/2
!             Get indices from connectivity matrix
              P1X = BRIDGE_BLOCK_LOC_RES(I)%VEC(1,J)
              P1Y = BRIDGE_BLOCK_LOC_RES(I)%VEC(2,J)
              DIST = 1.0E+30
              CLOSEST = 0
              DO K=1,NPOIN
                  XI1 = X(K)
                  YI1 = Y(K) 
                  DISTI = SQRT((P1X-XI1)**2.+(P1Y-YI1)**2.)
                  IF (DISTI < DIST) THEN
                      DIST = DISTI
                      CLOSEST = K
!                     For multiple processors get closest node
!                     of each of the processors
                      IF(NCSIZE.GT.1) THEN
                          GLOBCLOSE = MESH%KNOLG%I(CLOSEST)
                      ENDIF
!                     Direct node assignment for 1 processor
!                     multiple processors will be handled later
                      IF(NCSIZE.EQ.1) NODELIST(J)=CLOSEST
                  ENDIF
              DISTLIST(J) = DIST
              DISTTEMP(J) = DIST
              NODETEMP(J) = GLOBCLOSE
              ENDDO
          ENDDO
!         Merge global nodes from CPU
          DO J = 1, SIZE(NODELIST)
              ! save distances to temp cpu variable
              TEMPCPU = DISTLIST(J)
              ! proceed with min distance from any cpu
              IF(NCSIZE.GT.1) DISTLIST(J)=P_MIN(TEMPCPU)
              ! set all nodes with larger distances to 0
              IF (ABS(DISTLIST(J)-DISTTEMP(J))>1.0E-5) THEN
                  NODETEMP(J) = 0
              ENDIF
              TEMPINT = NODETEMP(J)
              ! Retrieve global nodelist for multiple processors
              IF(NCSIZE.GT.1) NODELIST(J)=P_MAX(TEMPINT)       
          ENDDO
!         remove duplicates from nodelist
          DO J = 1, SIZE(NODELIST)
              DO K = 1, J - 1
                  IF (NODELIST(J) == NODELIST(K)) THEN
                     MK3(J) = .FALSE.
                     EXIT
                  ENDIF
              ENDDO
          ENDDO
!         Packs the list without duplicates
          UNILIST = PACK(NODELIST, MK3)
!         Check if the nodelist contains any 3 point pairs
!         belonging to a single element
          DO J = 1, SIZE(UNILIST)
              CONTALL = .FALSE.
              DO K = 1, NELMAX
!                 Special treatment for 1 processor
                  IF(NCSIZE.EQ.1) THEN
                      I1G = IKLE(K,1)
                      I2G = IKLE(K,2)
                      I3G = IKLE(K,3)
                  ELSE
                      I1 = IKLE(K,1)
                      I1G = MESH%KNOLG%I(I1)
                      I2 = IKLE(K,2)
                      I2G = MESH%KNOLG%I(I2)
                      I3 = IKLE(K,3)
                      I3G = MESH%KNOLG%I(I3)
                  ENDIF
!                 check if one node is not present
                  IF (I1G == UNILIST(J) .or. I2G == UNILIST(J) 
     &                .or. I3G == UNILIST(J)) THEN
                      IF (ANY(UNILIST == I1G)) THEN
                          IF (ANY(UNILIST == I2G)) THEN
                              IF (ANY(UNILIST == I3G)) THEN
                                  CONTALL = .TRUE.
!                                 If true find the indices in the list
                                  IND1 = 0
                                  DO L = 1, SIZE(UNILIST)
                                      IF (I1G == UNILIST(L)) THEN
                                          IND1 = L
                                          EXIT
                                      ENDIF
                                  ENDDO
                                  IND2 = 0
                                  DO L = 1, SIZE(UNILIST)
                                      IF (I2G == UNILIST(L)) THEN
                                          IND2 = L
                                          EXIT
                                      ENDIF
                                  ENDDO
                                  IND3 = 0
                                  DO L = 1, SIZE(UNILIST)
                                      IF (I3G == UNILIST(L)) THEN
                                          IND3 = L
                                          EXIT
                                      ENDIF
                                  ENDDO
                              ENDIF
                          ENDIF
                      ENDIF
                  ENDIF         
              ENDDO
              IF (CONTALL .eqv. .TRUE.) THEN
!                 Find index to remove
!                 Only allow shortcutting if all points
!                 lie next to each other within the 1D array
!                 This is true if the helper variable is equal to 3
                  HELPER = (REAL(IND1)+REAL(IND2)+REAL(IND3)) /
     &                  ((REAL(IND1)+REAL(IND2)+REAL(IND3))/3D0)
!                 Check if points are next to each other and set
!                 points to zero
                  IF (HELPER == 3D0) THEN
                       INDDEL = (IND1 + IND2 + IND3) / 3
                       UNILIST(INDDEL) = 0
                  ENDIF
              ENDIF
          ENDDO
!         Merge unique list from processors
          DO J = 1, SIZE(UNILIST)
              TEMPINT = UNILIST(J)
              IF(NCSIZE.GT.1) UNILIST(J)=P_MIN(TEMPINT)
          ENDDO
!         Build nodestring from list of uniques, we exclude 0
!         Remove 0 from unique list
          allocate(MK4(SIZE(UNILIST)))
          MK4(:) = .TRUE.         
          DO J = 1, SIZE(UNILIST)
              IF (UNILIST(J) .EQ. 0) THEN
                  MK4(J) = .FALSE.
              ENDIF
          ENDDO
          UNICLEAN = PACK(UNILIST, MK4)
!         Build nodestring
          allocate(NODESTRING(I)%VEC(2,SIZE(UNICLEAN)-1))
          DO J = 1, SIZE(UNICLEAN)-1
              NODESTRING(I)%VEC(1,J) = UNICLEAN(J)
              NODESTRING(I)%VEC(2,J) = UNICLEAN(J+1)
          ENDDO
          
!         Provide nodestring also as local coordinates
          allocate(NODELOCAL(I)%VEC(2,SIZE(UNICLEAN)-1))
          NODELOCAL(I)%VEC(:,:) = 0
          DO J = 1, SIZE(UNICLEAN)-1
              NODELOCAL(I)%VEC(1,J) = NODESTRING(I)%VEC(1,J)
              NODELOCAL(I)%VEC(2,J) = NODESTRING(I)%VEC(2,J)
              IF (NCSIZE.GT.1) THEN
                  NODELOCAL(I)%VEC(1,J) = 
     &     GLOBAL_TO_LOCAL_POINT(NODESTRING(I)%VEC(1,J),MESH)
                  NODELOCAL(I)%VEC(2,J) = 
     &     GLOBAL_TO_LOCAL_POINT(NODESTRING(I)%VEC(2,J),MESH)
              ENDIF
          ENDDO
!         Find elements that belong to the segments upstream
!         and downstream
          allocate(BNEIGHGLOB(I)%VEC(6,SIZE(UNICLEAN)-1))
          BNEIGHGLOB(I)%VEC(:,:) = 0
!         allocate distances
          allocate(DISTTRI(I)%VEC(1,SIZE(UNICLEAN)-1))
          DISTTRI(I)%VEC(:,:) = 0
          DO J=1, SIZE(NODESTRING(I)%VEC) / 2
!             Retrieve global node pairs
              LOC1 = NODESTRING(I)%VEC(1,J)
              LOC2 = NODESTRING(I)%VEC(2,J)
!             Loop over elements
              DIST = 0
              DO K = 1, NELMAX
!                 Special treatment for 1 processor
                  IF(NCSIZE.EQ.1) THEN
                      I1 = IKLE(K,1)
                      I2 = IKLE(K,2)
                      I3 = IKLE(K,3)
                      I1G = IKLE(K,1)
                      I2G = IKLE(K,2)
                      I3G = IKLE(K,3)
                  ELSE
                      I1 = IKLE(K,1)
                      I1G = MESH%KNOLG%I(I1)
                      I2 = IKLE(K,2)
                      I2G = MESH%KNOLG%I(I2)
                      I3 = IKLE(K,3)
                      I3G = MESH%KNOLG%I(I3)
                  ENDIF
              IF (LOC1.EQ.I1G.OR.
     &                LOC1.EQ.I2G.OR.LOC1.EQ.I3G) THEN
                  IF (LOC2.EQ.I1G.OR.
     &                    LOC2.EQ.I2G.OR.LOC2.EQ.I3G) THEN
!                     compute cross product
                      IF (NCSIZE.EQ.1) THEN
                          X1 = X(LOC1)
                          X2 = X(LOC2)
                          Y1 = Y(LOC1)
                          Y2 = Y(LOC2)
                      ELSE
                          IF (GLOBAL_TO_LOCAL_POINT(LOC1,MESH)
     &                          .NE. 0) THEN
                          IF (GLOBAL_TO_LOCAL_POINT(LOC2,MESH)
     &                          .NE. 0) THEN
                          X1 = X(GLOBAL_TO_LOCAL_POINT(LOC1,MESH))
                          X2 = X(GLOBAL_TO_LOCAL_POINT(LOC2,MESH))
                          Y1 = Y(GLOBAL_TO_LOCAL_POINT(LOC1,MESH))
                          Y2 = Y(GLOBAL_TO_LOCAL_POINT(LOC2,MESH))
                          ENDIF
                          ENDIF
                      ENDIF
!                     Compute centers
                      XX1 = (X(I1) + X(I2) + X(I3)) / 3D0
                      YY1 = (Y(I1) + Y(I2) + Y(I3)) / 3D0
!                     compute cross product                     
                      CROSSP = (X2-X1)*(YY1-Y1)-(Y2-Y1)*(XX1-X1)
!                     Check on which side the point lies
                      IF (CROSSP .LE. 0D0) THEN
                         FILLED = 1
                      ENDIF
                      IF (CROSSP .GT. 0D0) THEN
                         FILLED = 4
                      ENDIF
!                     Compute and add distance
                      DIST = DIST + 
     &                    ((XX1-X1)**2D0+(YY1-Y1)**2D0)**(1D0/2D0)
                      DISTTRI(I)%VEC(1,J) = DIST
!                     Add to neighbor segments
                      BNEIGHGLOB(I)%VEC(FILLED,J) = I1G
                      BNEIGHGLOB(I)%VEC(FILLED + 1,J) = I2G
                      BNEIGHGLOB(I)%VEC(FILLED + 2,J) = I3G
                  ENDIF
              ENDIF
              ENDDO
          ENDDO
!         Merge neighbors and distances from processors if required
          IF (NCSIZE.GT.1) THEN
              DO J = 1, SIZE(UNICLEAN)-1
                  DO K=1,6
                      BNEIGHGLOB(I)%VEC(K,J) = 
     &                    P_MAX(BNEIGHGLOB(I)%VEC(K,J))
                  ENDDO
              ENDDO
              DO J = 1, SIZE(DISTTRI(I)%VEC)
                  DISTTRI(I)%VEC(1,J)=P_SUM(DISTTRI(I)%VEC(1,J))
              ENDDO
          ENDIF
!         Retrieve local node numbers for neighbors
          allocate(BNEIGHLOC(I)%VEC(6,SIZE(UNICLEAN)-1))
          BNEIGHLOC(I)%VEC(:,:) = 0
          DO J = 1, SIZE(UNICLEAN)-1
              DO K=1,6
                  BNEIGHLOC(I)%VEC(K,J) = BNEIGHGLOB(I)%VEC(K,J)
                  IF (NCSIZE.GT.1) THEN
                      BNEIGHLOC(I)%VEC(K,J) = 
     &     GLOBAL_TO_LOCAL_POINT(BNEIGHGLOB(I)%VEC(K,J),MESH)
                  ENDIF
              ENDDO
          ENDDO
!         build upstream and downstream nodes
          allocate(NPAIRLOC(I)%VEC(2,SIZE(UNICLEAN)-1))
          allocate(NPAIRGLOB(I)%VEC(2,SIZE(UNICLEAN)-1))
          NPAIRGLOB(I)%VEC(:,:) = 0
!         build global upstream and downstream
          DO J=1, SIZE(UNICLEAN)-1
              POSFILLED = 1
              DO K = 1,6
                  CON2 = .FALSE.
                  DO L = 1,6
                      IF (BNEIGHGLOB(I)%VEC(K,J) 
     &                    .EQ. BNEIGHGLOB(I)%VEC(L,J)) THEN
                              IF (K .NE. L) THEN
                                  CON2 = .TRUE.
                              ENDIF
                      ENDIF
                  ENDDO
                  IF (CON2 .EQV. .FALSE.) THEN
                      NPAIRGLOB(I)%VEC(POSFILLED,J) = 
     &                    BNEIGHGLOB(I)%VEC(K,J)
                      POSFILLED = POSFILLED + 1
                  ENDIF
              ENDDO
          ENDDO
!         build local upstream and downstream
          DO J = 1, SIZE(UNICLEAN)-1
              IF (NCSIZE.GT.1) THEN
                  NPAIRLOC(I)%VEC(1,J) =
     &     GLOBAL_TO_LOCAL_POINT(NPAIRGLOB(I)%VEC(1,J),MESH)
                  NPAIRLOC(I)%VEC(2,J) =
     &     GLOBAL_TO_LOCAL_POINT(NPAIRGLOB(I)%VEC(2,J),MESH)
              ELSE
                  NPAIRLOC(I)%VEC(1,J) =
     &     NPAIRGLOB(I)%VEC(1,J)
                  NPAIRLOC(I)%VEC(2,J) =
     &     NPAIRGLOB(I)%VEC(2,J)
              ENDIF
          ENDDO
!         deallocate helper arrays
          deallocate(NODELIST)
          deallocate(MK3)
          deallocate(MK4)
          deallocate(DISTLIST)
          deallocate(DISTTEMP)
          deallocate(NODETEMP)
      ENDDO   
      
         
!     Write a message for finishing
      WRITE(LU,*) 'Finished Initializing Bridge Structures'

!
      RETURN
      END
!
