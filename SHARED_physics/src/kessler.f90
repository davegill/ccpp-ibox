!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CCPP-compliant physics scheme template
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! General rules:
!
! - scheme must be in its own module (module name = scheme name) and must
!   have three entry points (subroutines) starting with the name of the module:
!   module scheme_template -> subroutines scheme_template_{init,finalize,run}
!
! - empty schemes (e.g., scheme_template_init below) do not need an argument table
!
! - schemes in use require an argument table as below; order of arguments in the
!   table must be the same as in the argument list of the subroutine
!
! - all external information required by the scheme must be passed in via the
!   argument list, i.e. NO 'use EXTERNAL_MODULE' statements
!
! - if width of argument tables exceeds 250 characters, wrap the table (but only
!   the table) in CPP preprocessor directives #if 0 YOUR_TABLE #endif
!
! - for readibility, it is suggested to align the columns in the metadata table
!
! Input/output variable (argument) rules:
!
! - for a list of variables available for the specific host model, see table
!   "TABLE_NAME_NUMBER_MISSING" [howto keep up to date?] in the CCPP developer's guide
!
! - a standard_name cannot be assigned to more than one local variable (local_name)
!
! - all information (units, rank, index ordering) must match the specifications
!   on the host model side, but subslices can be used/added in the host model:
!   HOST MODEL: real, dimension(:,:,:,:) :: hydrometeors
!
!
! Coding rules:
!
! - code must comply to modern Fortran standards (Fortran 90/95/2003)
!
! - use labeled 'end' statements for modules, subroutines and functions
!   module scheme_template -> end module scheme_template
!
! - use implicit none
!
! - all intent(out) variables must be initialized properly inside the subroutine
!
! - NO permanent state inside the module, i.e. no variables carrying the 'save' attribute
!
! - NO 'goto' statements
!
! - errors are handled by the host model using the two mandatory arguments
!   errmsg and errflg; in the event of an error, assign a meaningful error
!   message to errmsg and set errflg to a value other than 0
!
! - schemes are NOT allowed to abort/stop the program
!
! - schemes are NOT allowed to perform I/O operations (except for reading
!   lookup tables / other information needed to initialize the scheme)
!
! - line lengths of 120 characters are suggested for better readibility
!   (exception: CCPP metadata argument tables)
!
! Parallel programming rules:
!
! - if OpenMP is used, the number of allowed threads must be provided by the
!   host model as an intent(in) argument in the argument list
!
! - if MPI is used, it is restricted to global communications: barrier, broadcast,
!   gather, scatter, reduction; the MPI communicator must be provided by the
!   host model as an intent(in) argument in the argument list
!   - do NOT use MPI_COMM_WORLD
!   - do NOT use any point-to-point communication
!
! - if Fortran coarrays are used, consult with the CCPP development team
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!WRF:MODEL_LAYER:PHYSICS
!

MODULE kessler

  PRIVATE
  PUBLIC :: KESSLER_INIT 
  PUBLIC :: KESSLER_RUN
  PUBLIC :: KESSLER_FINALIZE

CONTAINS

!> \section arg_table_kessler_init  Argument Table
!! | local_name | standard_name                                    | long_name                               | units       | rank | type      | kind      | intent | optional |
!! |------------|--------------------------------------------------|-----------------------------------------|-------------|------|-----------|-----------|--------|----------|
!! | errmsg     | error_message                                    | CCPP error message                      | none        |    0 | character | len=512   | out    | F        |
!! | errflg     | error_flag                                       | CCPP error flag                         | flag        |    0 | integer   |           | out    | F        |
!!
  subroutine kessler_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg
    
    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine kessler_init

!> \section arg_table_kessler_init_HMM Argument Table
!! | local_name | standard_name    | long_name            | units | rank | type      | kind      | intent | optional |
!! |------------|------------------|----------------------|-------|------|-----------|-----------|--------|----------|
!! | t          | theta            | potential_temp       | K     |    3 | real      | 4         | out    | F        |
!! | qv         | qvapor           | vapor_mixing_ratio   | kg/kg |    3 | real      | 4         | out    | F        |
!! | qc         | qcloud           | cloud_mixing_ratio   | kg/kg |    3 | real      | 4         | out    | F        |
!! | qr         | qrain            | rain_mixing_ratio    | kg/kg |    3 | real      | 4         | out    | F        |
!! | rho        | density          | dry_air_density      | kg/m^3|    3 | real      | 4         | out    | F        |
!! | pii        | exner            | exner_function       |dimless|    3 | real      | 4         | out    | F        |
!! | dt_in      | dt               | time_step            | s     |    0 | real      | 4         | out    | F        |
!! | z          | height           | height_above_msl     | m     |    3 | real      | 4         | out    | F        |
!! | xlv        | xlv              | latent_heat_melting  | J/kg  |    0 | real      | 4         | out    | F        |
!! | cp         | cp               | heat_cap_const_pres  | J/K   |    0 | real      | 4         | out    | F        |
!! | ep2        | Rd_over_Rv       | Rd_over_Rv           |dimless|    0 | real      | 4         | out    | F        |
!! | svp1       | svp1             | svp1                 | ??    |    0 | real      | 4         | out    | F        |
!! | svp2       | svp2             | svp2                 | ??    |    0 | real      | 4         | out    | F        |
!! | svp3       | svp3             | svp3                 | ??    |    0 | real      | 4         | out    | F        |
!! | svpt0      | svpt0            | svpt0                | ??    |    0 | real      | 4         | out    | F        |
!! | rhowater   | density_water    | density_water        | kg/m^3|    0 | real      | 4         | out    | F        |
!! | dz8w       | dz               | delta_zfull          | m/m   |    3 | real      | 4         | out    | F        |
!! | rainnc     | rainnc           | accum_mp_precip      | kg/m^2|    2 | real      | 4         | out    | F        |
!! | rainncv    | rainnc_per_dt    | dt_mp_precip         | kg/m^2|    2 | real      | 4         | out    | F        |
!! | ids        | dom_h3121_start  | dom_h3121_start      | dim   |    0 | integer   | 4         | in    | F        |
!! | ide        | dom_h3121_end    | dom_h3121_end        | dim   |    0 | integer   | 4         | in    | F        |
!! | jds        | dom_h3322_start  | dom_h3322_start      | dim   |    0 | integer   | 4         | in    | F        |
!! | jde        | dom_h3322_end    | dom_h3322_end        | dim   |    0 | integer   | 4         | in    | F        |
!! | kds        | dom_v3211_start  | dom_v3211_start      | dim   |    0 | integer   | 4         | in    | F        |
!! | kde        | dom_v3211_end    | dom_v3211_end        | dim   |    0 | integer   | 4         | in    | F        |
!! | ims        | mem_h3121_start  | mem_h3121_start      | dim   |    0 | integer   | 4         | in    | F        |
!! | ime        | mem_h3121_end    | mem_h3121_end        | dim   |    0 | integer   | 4         | in    | F        |
!! | jms        | mem_h3322_start  | mem_h3322_start      | dim   |    0 | integer   | 4         | in    | F        |
!! | jme        | mem_h3322_end    | mem_h3322_end        | dim   |    0 | integer   | 4         | in    | F        |
!! | kms        | mem_v3211_start  | mem_v3211_start      | dim   |    0 | integer   | 4         | in    | F        |
!! | kme        | mem_v3211_end    | mem_v3211_end        | dim   |    0 | integer   | 4         | in    | F        |
!! | its        |tile_h3121_start  |tile_h3121_start      | dim   |    0 | integer   | 4         | in    | F        |
!! | ite        |tile_h3121_end    |tile_h3121_end        | dim   |    0 | integer   | 4         | in    | F        |
!! | jts        |tile_h3322_start  |tile_h3322_start      | dim   |    0 | integer   | 4         | in    | F        |
!! | jte        |tile_h3322_end    |tile_h3322_end        | dim   |    0 | integer   | 4         | in    | F        |
!! | kts        |tile_v3211_start  |tile_v3211_start      | dim   |    0 | integer   | 4         | in    | F        |
!! | kte        |tile_v3211_end    |tile_v3211_end        | dim   |    0 | integer   | 4         | in    | F        |
!! | errmsg     | error_message    | CCPP error message   | none  |    0 | character | len=512   | out    | F        |
!! | errflg     | error_flag       | CCPP error flag      | flag  |    0 | integer   |           | out    | F        |
!!
   SUBROUTINE kessler_init_HMM( t, qv, qc, qr, rho, pii                  &
                           ,dt_in, z, xlv, cp                        &
                           ,EP2,SVP1,SVP2,SVP3,SVPT0,rhowater        &
                           ,dz8w                                     &
                           ,RAINNC, RAINNCV                          &
                           ,ids,ide, jds,jde, kds,kde                & ! domain dims
                           ,ims,ime, jms,jme, kms,kme                & ! memory dims
                           ,its,ite, jts,jte, kts,kte                & ! tile   dims
                           ,errmsg, errflg)
      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
      REAL   ,      INTENT(OUT  )    :: xlv, cp
      REAL   ,      INTENT(OUT  )    :: EP2,SVP1,SVP2,SVP3,SVPT0
      REAL   ,      INTENT(OUT  )    :: rhowater

      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
            INTENT(OUT) ::                                       &
                                                               t , &
                                                               qv, &
                                                               qc, &
                                                               qr

      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
            INTENT(OUT  ) ::                                       &
                                                              rho, &
                                                              pii, &
                                                             dz8w 

      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
            INTENT(OUT  ) ::                                    z

      REAL, INTENT(OUT  ) :: dt_in

      REAL, DIMENSION( ims:ime , jms:jme ),                        &
            INTENT(OUT) ::                               RAINNC, &
                                                          RAINNCV
      character(len=512),      intent(out)   :: errmsg
      integer,                 intent(out)   :: errflg

      call get_data( t, qv, qc, qr, rho, pii                  &
               ,dt_in, z, xlv, cp                        &
               ,EP2,SVP1,SVP2,SVP3,SVPT0,rhowater        &
               ,dz8w                                     &
               ,RAINNC, RAINNCV                          &
               ,ids,ide, jds,jde, kds,kde                & ! domain dims
               ,ims,ime, jms,jme, kms,kme                & ! memory dims
               ,its,ite, jts,jte, kts,kte)                 ! tile   dims

   END SUBROUTINE kessler_init_HMM

!> \section arg_table_kessler_run Argument Table
!! | local_name | standard_name    | long_name            | units | rank | type      | kind      | intent | optional |
!! |------------|------------------|----------------------|-------|------|-----------|-----------|--------|----------|
!! | t          | theta            | potential_temp       | K     |    3 | real      | 4         | inout  | F        |
!! | qv         | qvapor           | vapor_mixing_ratio   | kg/kg |    3 | real      | 4         | inout  | F        |
!! | qc         | qcloud           | cloud_mixing_ratio   | kg/kg |    3 | real      | 4         | inout  | F        |
!! | qr         | qrain            | rain_mixing_ratio    | kg/kg |    3 | real      | 4         | inout  | F        |
!! | rho        | density          | dry_air_density      | kg/m^3|    3 | real      | 4         | in     | F        |
!! | pii        | exner            | exner_function       |dimless|    3 | real      | 4         | in     | F        |
!! | dt_in      | dt               | time_step            | s     |    0 | real      | 4         | in     | F        |
!! | z          | height           | height_above_msl     | m     |    3 | real      | 4         | in     | F        |
!! | xlv        | xlv              | latent_heat_melting  | J/kg  |    0 | real      | 4         | in     | F        |
!! | cp         | cp               | heat_cap_const_pres  | J/K   |    0 | real      | 4         | in     | F        |
!! | ep2        | Rd_over_Rv       | Rd_over_Rv           |dimless|    0 | real      | 4         | in     | F        |
!! | svp1       | svp1             | svp1                 | ??    |    0 | real      | 4         | in     | F        |
!! | svp2       | svp2             | svp2                 | ??    |    0 | real      | 4         | in     | F        |
!! | svp3       | svp3             | svp3                 | ??    |    0 | real      | 4         | in     | F        |
!! | svpt0      | svpt0            | svpt0                | ??    |    0 | real      | 4         | in     | F        |
!! | rhowater   | density_water    | density_water        | kg/m^3|    0 | real      | 4         | in     | F        |
!! | dz8w       | dz               | delta_zfull          | m/m   |    3 | real      | 4         | in     | F        |
!! | rainnc     | rainnc           | accum_mp_precip      | kg/m^2|    2 | real      | 4         | inout  | F        |
!! | rainncv    | rainnc_per_dt    | dt_mp_precip         | kg/m^2|    2 | real      | 4         | inout  | F        |
!! | ids        | dom_h3121_start  | dom_h3121_start      | dim   |    0 | integer   | 4         | in     | F        |
!! | ide        | dom_h3121_end    | dom_h3121_end        | dim   |    0 | integer   | 4         | in     | F        |
!! | jds        | dom_h3322_start  | dom_h3322_start      | dim   |    0 | integer   | 4         | in     | F        |
!! | jde        | dom_h3322_end    | dom_h3322_end        | dim   |    0 | integer   | 4         | in     | F        |
!! | kds        | dom_v3211_start  | dom_v3211_start      | dim   |    0 | integer   | 4         | in     | F        |
!! | kde        | dom_v3211_end    | dom_v3211_end        | dim   |    0 | integer   | 4         | in     | F        |
!! | ims        | mem_h3121_start  | mem_h3121_start      | dim   |    0 | integer   | 4         | in     | F        |
!! | ime        | mem_h3121_end    | mem_h3121_end        | dim   |    0 | integer   | 4         | in     | F        |
!! | jms        | mem_h3322_start  | mem_h3322_start      | dim   |    0 | integer   | 4         | in     | F        |
!! | jme        | mem_h3322_end    | mem_h3322_end        | dim   |    0 | integer   | 4         | in     | F        |
!! | kms        | mem_v3211_start  | mem_v3211_start      | dim   |    0 | integer   | 4         | in     | F        |
!! | kme        | mem_v3211_end    | mem_v3211_end        | dim   |    0 | integer   | 4         | in     | F        |
!! | its        |tile_h3121_start  |tile_h3121_start      | dim   |    0 | integer   | 4         | in     | F        |
!! | ite        |tile_h3121_end    |tile_h3121_end        | dim   |    0 | integer   | 4         | in     | F        |
!! | jts        |tile_h3322_start  |tile_h3322_start      | dim   |    0 | integer   | 4         | in     | F        |
!! | jte        |tile_h3322_end    |tile_h3322_end        | dim   |    0 | integer   | 4         | in     | F        |
!! | kts        |tile_v3211_start  |tile_v3211_start      | dim   |    0 | integer   | 4         | in     | F        |
!! | kte        |tile_v3211_end    |tile_v3211_end        | dim   |    0 | integer   | 4         | in     | F        |
!! | errmsg     | error_message    | CCPP error message   | none  |    0 | character | len=512   | out    | F        |
!! | errflg     | error_flag       | CCPP error flag      | flag  |    0 | integer   |           | out    | F        |
!!
   SUBROUTINE kessler_run( t, qv, qc, qr, rho, pii                  &
                          ,dt_in, z, xlv, cp                        &
                          ,EP2,SVP1,SVP2,SVP3,SVPT0,rhowater        &
                          ,dz8w                                     &
                          ,RAINNC, RAINNCV                          &
                          ,ids,ide, jds,jde, kds,kde                & ! domain dims
                          ,ims,ime, jms,jme, kms,kme                & ! memory dims
                          ,its,ite, jts,jte, kts,kte                & ! tile   dims
                          ,errmsg, errflg)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------
   !  taken from the COMMAS code - WCS 10 May 1999.
   !  converted from FORTRAN 77 to 90, tiled, WCS 10 May 1999.
!----------------------------------------------------------------
   REAL    , PARAMETER ::  c1 = .001 
   REAL    , PARAMETER ::  c2 = .001 
   REAL    , PARAMETER ::  c3 = 2.2 
   REAL    , PARAMETER ::  c4 = .875 
   REAL    , PARAMETER ::  fudge = 1.0 
   REAL    , PARAMETER ::  mxfall = 10.0 
!----------------------------------------------------------------
   INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte
   REAL   ,      INTENT(IN   )    :: xlv, cp
   REAL   ,      INTENT(IN   )    :: EP2,SVP1,SVP2,SVP3,SVPT0
   REAL   ,      INTENT(IN   )    :: rhowater

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(INOUT) ::                                       &
                                                            t , &
                                                            qv, &
                                                            qc, &
                                                            qr

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(IN   ) ::                                       &
                                                           rho, &
                                                           pii, &
                                                          dz8w 

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(IN   ) ::                                    z

   REAL, INTENT(IN   ) :: dt_in

   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(INOUT) ::                               RAINNC, &
                                                       RAINNCV
    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg



   ! local variables

   REAL :: qrprod, ern, gam, rcgs, rcgsi
   REAL, DIMENSION( its:ite , kts:kte, jts:jte ) ::     prod
   REAL, DIMENSION(kts:kte) :: vt, prodk, vtden,rdzk,rhok,factor,rdzw
   INTEGER :: i,j,k
   INTEGER :: nfall, n, nfall_new
   REAL    :: qrr, pressure, temp, es, qvs, dz, dt
   REAL    :: f5, dtfall, rdz, product
   REAL    :: max_heating, max_condense, max_rain, maxqrp
   REAL    :: vtmax, ernmax, crmax, factorn, time_sediment
   REAL    :: qcr, factorr, ppt
   REAL, PARAMETER :: max_cr_sedimentation = 0.75
!----------------------------------------------------------------

   INTEGER :: imax, kmax
  
    errmsg = ''
    errflg = 0
    
    ! Check for an error, and if found set error flag and error message
    do j = jts,min(jde-1,jte)
    do k = kts,kte
    do i = its,min(ids-1,ide)
       if ( ( t   (i,k,j) .le. 0 ) .or. &
            ( qv  (i,k,j) .lt. 0 ) .or. &
            ( qc  (i,k,j) .lt. 0 ) .or. &
            ( qr  (i,k,j) .lt. 0 ) .or. &
            ( rho (i,k,j) .le. 0 ) .or. &
            ( pii (i,k,j) .le. 0 ) .or. &
            ( dz8w(i,k,j) .le. 0 ) ) then
          errflg = 1
          write(errmsg,fmt='(a,3(1x,i4),a,7(1x,A,g12.5))') 'Kessler_run, bad input values: (i,j,k) = (',i,j,k,')',&
              't    = ',t   (i,k,j) , &
              'qv   = ',qv  (i,k,j) , &
              'qc   = ',qc  (i,k,j) , &
              'qr   = ',qr  (i,k,j) , &
              'rho  = ',rho (i,k,j) , &
              'pii  = ',pii (i,k,j) , &
              'dz8w = ',dz8w(i,k,j)
          return
       end if
    end do
    end do
    end do

!call make_data( t, qv, qc, qr, rho, pii                  &
!               ,dt_in, z, xlv, cp                        &
!               ,EP2,SVP1,SVP2,SVP3,SVPT0,rhowater        &
!               ,dz8w                                     &
!               ,RAINNC, RAINNCV,                         &
!               ims,ime,jms,jme,kms,kme,                  &
!               51,27,5,17)

    dt = dt_in

!   f5 = 237.3 * 17.27 * 2.5e6 / cp 
    f5 = svp2*(svpt0-svp3)*xlv/cp
    ernmax = 0.
    maxqrp = -100.

!------------------------------------------------------------------------------
! parameters for the time split terminal advection
!------------------------------------------------------------------------------

      max_heating = 0.
      max_condense = 0.
      max_rain = 0.

!-----------------------------------------------------------------------------
! outer J loop for entire microphysics, outer i loop for sedimentation
!-----------------------------------------------------------------------------

  microphysics_outer_j_loop: DO j = jts, jte

  sedimentation_outer_i_loop: DO i = its,ite

!   vtmax = 0.
   crmax = 0.


!------------------------------------------------------------------------------
! Terminal velocity calculation and advection, set up coefficients and
! compute stable timestep
!------------------------------------------------------------------------------

   DO k = 1, kte
     prodk(k)   = qr(i,k,j)
     rhok(k) = rho(i,k,j)
     qrr = amax1(0.,qr(i,k,j)*0.001*rhok(k))
     vtden(k) = sqrt(rhok(1)/rhok(k))
     vt(k) = 36.34*(qrr**0.1364) * vtden(k)
!     vtmax = amax1(vt(k), vtmax)
     rdzw(k) = 1./dz8w(i,k,j)
     crmax = amax1(vt(k)*dt*rdzw(k),crmax)
   ENDDO
   DO k = 1, kte-1
     rdzk(k) = 1./(z(i,k+1,j) - z(i,k,j))
   ENDDO
   rdzk(kte) = 1./(z(i,kte,j) - z(i,kte-1,j))

   nfall = max(1,nint(0.5+crmax/max_cr_sedimentation))  ! courant number for big timestep.
   dtfall = dt / float(nfall)                           ! splitting so courant number for sedimentation
   time_sediment = dt                                   ! is stable

!------------------------------------------------------------------------------
! Terminal velocity calculation and advection
! Do a time split loop on this for stability.
!------------------------------------------------------------------------------

   column_sedimentation: DO WHILE ( nfall > 0 )

   time_sediment = time_sediment - dtfall
   DO k = 1, kte-1
     factor(k) = dtfall*rdzk(k)/rhok(k)
   ENDDO
   factor(kte) = dtfall*rdzk(kte)

   ppt=0.

      k = 1
      ppt=rhok(k)*prodk(k)*vt(k)*dtfall/rhowater
      RAINNCV(i,j)=ppt*1000.
      RAINNC(i,j)=RAINNC(i,j)+ppt*1000.  ! unit = mm
 
!------------------------------------------------------------------------------
! Time split loop, Fallout done with flux upstream
!------------------------------------------------------------------------------

      DO k = kts, kte-1
        prodk(k) = prodk(k) - factor(k)           &
                  * (rhok(k)*prodk(k)*vt(k)       &
                    -rhok(k+1)*prodk(k+1)*vt(k+1))
      ENDDO

      k = kte
      prodk(k) = prodk(k) - factor(k)*prodk(k)*vt(k)

!------------------------------------------------------------------------------
! compute new sedimentation velocity, and check/recompute new 
! sedimentation timestep if this isn't the last split step.
!------------------------------------------------------------------------------

      IF( nfall > 1 ) THEN ! this wasn't the last split sedimentation timestep

        nfall = nfall - 1
        crmax = 0.
        DO k = kts, kte 
          qrr = amax1(0.,prodk(k)*0.001*rhok(k))
          vt(k) = 36.34*(qrr**0.1364) * vtden(k)
!          vtmax = amax1(vt(k), vtmax)
          crmax = amax1(vt(k)*time_sediment*rdzw(k),crmax)
        ENDDO

        nfall_new = max(1,nint(0.5+crmax/max_cr_sedimentation))
        if (nfall_new /= nfall ) then
          nfall = nfall_new
          dtfall = time_sediment/nfall
        end if

      ELSE  ! this was the last timestep

        DO k=kts,kte
          prod(i,k,j) = prodk(k)
        ENDDO
        nfall = 0  ! exit condition for sedimentation loop

      END IF

   ENDDO column_sedimentation

   ENDDO sedimentation_outer_i_loop

!------------------------------------------------------------------------------
! Production of rain and deletion of qc
! Production of qc from supersaturation
! Evaporation of QR
!------------------------------------------------------------------------------

     DO k = kts, kte
     DO i = its, ite
       factorn = 1.0 / (1.+c3*dt*amax1(0.,qr(i,k,j))**c4)
       qrprod = qc(i,k,j) * (1.0 - factorn)           &
             + factorn*c1*dt*amax1(qc(i,k,j)-c2,0.)      
       rcgs = 0.001*rho(i,k,j)

       qc(i,k,j) = amax1(qc(i,k,j) - qrprod,0.)
       qr(i,k,j) = (qr(i,k,j) + prod(i,k,j)-qr(i,k,j))
       qr(i,k,j) = amax1(qr(i,k,j) + qrprod,0.)

       temp      = pii(i,k,j)*t(i,k,j)
       pressure = 1.000e+05 * (pii(i,k,j)**(1004./287.))
       gam = 2.5e+06/(1004.*pii(i,k,j))
!      qvs       = 380.*exp(17.27*(temp-273.)/(temp- 36.))/pressure
       es        = 1000.*svp1*exp(svp2*(temp-svpt0)/(temp-svp3))
       qvs       = ep2*es/(pressure-es)
!      prod(i,k,j) = (qv(i,k,j)-qvs) / (1.+qvs*f5/(temp-36.)**2)
       prod(i,k,j) = (qv(i,k,j)-qvs) / (1.+pressure/(pressure-es)*qvs*f5/(temp-svp3)**2)
       ern  = amin1(dt*(((1.6+124.9*(rcgs*qr(i,k,j))**.2046)   &
          *(rcgs*qr(i,k,j))**.525)/(2.55e8/(pressure*qvs)       &
          +5.4e5))*(dim(qvs,qv(i,k,j))/(rcgs*qvs)),             &
          amax1(-prod(i,k,j)-qc(i,k,j),0.),qr(i,k,j))

! Update all variables

       product = amax1(prod(i,k,j),-qc(i,k,j))
       t (i,k,j) = t(i,k,j) + gam*(product - ern)
       qv(i,k,j) = amax1(qv(i,k,j) - product + ern,0.)
       qc(i,k,j) =       qc(i,k,j) + product
       qr(i,k,j) = qr(i,k,j) - ern

     ENDDO
     ENDDO

  ENDDO  microphysics_outer_j_loop
 call make_data( t, qv, qc, qr, rho, pii                  &
                ,dt_in, z, xlv, cp                        &
                ,EP2,SVP1,SVP2,SVP3,SVPT0,rhowater        &
                ,dz8w                                     &
                ,RAINNC, RAINNCV,                         &
                ims,ime,jms,jme,kms,kme,                  &
                51,27,5,18)

  RETURN

  END SUBROUTINE kessler_run

  subroutine kessler_finalize()
  end subroutine kessler_finalize

subroutine make_data( t, qv, qc, qr, rho, pii                  &
               ,dt_in, z, xlv, cp                        &
               ,EP2,SVP1,SVP2,SVP3,SVPT0,rhowater        &
               ,dz8w                                     &
               ,RAINNC, RAINNCV                          &
               ,ims,ime, jms,jme, kms,kme,  &
               i_center,j_center,ieach_side,file_unit)
implicit none
   INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
   INTEGER,      INTENT(IN   )    :: i_center,j_center,ieach_side,file_unit
   REAL   ,      INTENT(IN   )    :: xlv, cp
   REAL   ,      INTENT(IN   )    :: EP2,SVP1,SVP2,SVP3,SVPT0
   REAL   ,      INTENT(IN   )    :: rhowater

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(IN) ::                                       &
                                                            t , &
                                                            qv, &
                                                            qc, &
                                                            qr

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(IN   ) ::                                       &
                                                           rho, &
                                                           pii, &
                                                          dz8w 

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(IN   ) ::                                    z

   REAL, INTENT(IN   ) :: dt_in

   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(IN) ::                               RAINNC, &
                                                       RAINNCV

   INTEGER                        :: lids,lide, ljds,ljde, lkds,lkde, &
                                     lims,lime, ljms,ljme, lkms,lkme, &
                                     lits,lite, ljts,ljte, lkts,lkte

   lids = i_center-ieach_side
   lide = i_center+ieach_side+1
   lims = i_center-ieach_side
   lime = i_center+ieach_side
   lits = i_center-ieach_side
   lite = i_center+ieach_side

   ljds = j_center-ieach_side
   ljde = j_center+ieach_side+1
   ljms = j_center-ieach_side
   ljme = j_center+ieach_side
   ljts = j_center-ieach_side
   ljte = j_center+ieach_side

   lkds = kms
   lkde = kme
   lkms = kms
   lkme = kme
   lkts = kms
   lkte = kme-1

   WRITE(file_unit,*) 'DIMS'
   WRITE(file_unit,*) lids,lide,ljds,ljde,lkds,lkde,lims,lime,ljms,ljme,lkms,lkme,lits,lite,ljts,ljte,lkts,lkte

   WRITE(file_unit,*) 'T'
   WRITE(file_unit,*) t(i_center-ieach_side:i_center+ieach_side,kms:kme,j_center-ieach_side:j_center+ieach_side)

   WRITE(file_unit,*) 'QV'
   WRITE(file_unit,*) qv(i_center-ieach_side:i_center+ieach_side,kms:kme,j_center-ieach_side:j_center+ieach_side)

   WRITE(file_unit,*) 'QC'
   WRITE(file_unit,*) qc(i_center-ieach_side:i_center+ieach_side,kms:kme,j_center-ieach_side:j_center+ieach_side)

   WRITE(file_unit,*) 'QR'
   WRITE(file_unit,*) qr(i_center-ieach_side:i_center+ieach_side,kms:kme,j_center-ieach_side:j_center+ieach_side)

   WRITE(file_unit,*) 'RHO'
   WRITE(file_unit,*) rho(i_center-ieach_side:i_center+ieach_side,kms:kme,j_center-ieach_side:j_center+ieach_side)

   WRITE(file_unit,*) 'PII'
   WRITE(file_unit,*) pii(i_center-ieach_side:i_center+ieach_side,kms:kme,j_center-ieach_side:j_center+ieach_side)

   WRITE(file_unit,*) 'DT_IN'
   WRITE(file_unit,*) dt_in

   WRITE(file_unit,*) 'Z'
   WRITE(file_unit,*) z(i_center-ieach_side:i_center+ieach_side,kms:kme,j_center-ieach_side:j_center+ieach_side)

   WRITE(file_unit,*) 'XLV'
   WRITE(file_unit,*) xlv

   WRITE(file_unit,*) 'CP'
   WRITE(file_unit,*) cp

   WRITE(file_unit,*) 'EP2'
   WRITE(file_unit,*) ep2

   WRITE(file_unit,*) 'SVP1'
   WRITE(file_unit,*) svp1

   WRITE(file_unit,*) 'SVP2'
   WRITE(file_unit,*) svp2

   WRITE(file_unit,*) 'SVP3'
   WRITE(file_unit,*) svp3

   WRITE(file_unit,*) 'SVPT0'
   WRITE(file_unit,*) svpt0

   WRITE(file_unit,*) 'RHOWATER'
   WRITE(file_unit,*) rhowater

   WRITE(file_unit,*) 'DZ8W'
   WRITE(file_unit,*) dz8w(i_center-ieach_side:i_center+ieach_side,kms:kme,j_center-ieach_side:j_center+ieach_side)

   WRITE(file_unit,*) 'RAINNC'
   WRITE(file_unit,*) rainnc(i_center-ieach_side:i_center+ieach_side,j_center-ieach_side:j_center+ieach_side)

   WRITE(file_unit,*) 'RAINNCV'
   WRITE(file_unit,*) rainncv(i_center-ieach_side:i_center+ieach_side,j_center-ieach_side:j_center+ieach_side)

end subroutine make_data

END MODULE kessler

subroutine get_data( t, qv, qc, qr, rho, pii                  &
               ,dt_in, z, xlv, cp                        &
               ,EP2,SVP1,SVP2,SVP3,SVPT0,rhowater        &
               ,dz8w                                     &
               ,RAINNC, RAINNCV                          &
               ,ids,ide, jds,jde, kds,kde                & ! domain dims
               ,ims,ime, jms,jme, kms,kme                & ! memory dims
               ,its,ite, jts,jte, kts,kte)                 ! tile   dims
implicit none
   INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte
   REAL   ,      INTENT(out  )    :: xlv, cp
   REAL   ,      INTENT(out  )    :: EP2,SVP1,SVP2,SVP3,SVPT0
   REAL   ,      INTENT(out  )    :: rhowater

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(OUT) ::                                       &
                                                            t , &
                                                            qv, &
                                                            qc, &
                                                            qr

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(OUT  ) ::                                       &
                                                           rho, &
                                                           pii, &
                                                          dz8w 

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(OUT  ) ::                                    z

   REAL, INTENT(OUT  ) :: dt_in

   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(OUT) ::                               RAINNC, &
                                                       RAINNCV

   INTEGER                        :: lids,lide, ljds,ljde, lkds,lkde, &
                                     lims,lime, ljms,ljme, lkms,lkme, &
                                     lits,lite, ljts,ljte, lkts,lkte

   INTEGER :: file_unit 
   CHARACTER (LEN=80) :: file_name
   CHARACTER (LEN=32) :: string
   file_name = 'wrf_kessler_input.txt                                                           '
   !wrf_kessler_output.txt
   file_unit =17
   open (unit=file_unit,file=TRIM(file_name),status='old',form='formatted',access='sequential')
   rewind(file_unit)

   READ(file_unit,*) string
   READ(file_unit,*) lids,lide,ljds,ljde,lkds,lkde,lims,lime,ljms,ljme,lkms,lkme,lits,lite,ljts,ljte,lkts,lkte

   READ(file_unit,*) string
   READ(file_unit,*) t(ims:ime,kms:kme,jms:jme)

   READ(file_unit,*) string
   READ(file_unit,*) qv(ims:ime,kms:kme,jms:jme)

   READ(file_unit,*) string
   READ(file_unit,*) qc(ims:ime,kms:kme,jms:jme)

   READ(file_unit,*) string
   READ(file_unit,*) qr(ims:ime,kms:kme,jms:jme)

   READ(file_unit,*) string
   READ(file_unit,*) rho(ims:ime,kms:kme,jms:jme)

   READ(file_unit,*) string
   READ(file_unit,*) pii(ims:ime,kms:kme,jms:jme)

   READ(file_unit,*) string
   READ(file_unit,*) dt_in

   READ(file_unit,*) string
   READ(file_unit,*) z(ims:ime,kms:kme,jms:jme)

   READ(file_unit,*) string
   READ(file_unit,*) xlv

   READ(file_unit,*) string
   READ(file_unit,*) cp

   READ(file_unit,*) string
   READ(file_unit,*) ep2

   READ(file_unit,*) string
   READ(file_unit,*) svp1

   READ(file_unit,*) string
   READ(file_unit,*) svp2

   READ(file_unit,*) string
   READ(file_unit,*) svp3

   READ(file_unit,*) string
   READ(file_unit,*) svpt0

   READ(file_unit,*) string
   READ(file_unit,*) rhowater

   READ(file_unit,*) string
   READ(file_unit,*) dz8w(ims:ime,kms:kme,jms:jme)

   READ(file_unit,*) string
   READ(file_unit,*) rainnc(ims:ime,jms:jme)

   READ(file_unit,*) string
   READ(file_unit,*) rainncv(ims:ime,jms:jme)

   CLOSE(file_unit)

end subroutine get_data

subroutine how_big( ids,ide, jds,jde, kds,kde                & ! domain dims
                   ,ims,ime, jms,jme, kms,kme                & ! memory dims
                   ,its,ite, jts,jte, kts,kte)
      IMPLICIT NONE

      INTEGER,      INTENT(OUT  )    :: ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
      INTEGER :: file_unit 
      CHARACTER (LEN=80) :: file_name
      CHARACTER (LEN=32) :: string
      file_name = 'wrf_kessler_input.txt                                                           '
      file_unit =17
      open (unit=file_unit,file=TRIM(file_name),status='old',form='formatted',access='sequential')

      READ(file_unit,*) string
      READ(fiLe_unit,*) ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
      CLOSE (file_unit)

end subroutine how_big
