!> \file phystest_type_defs.f90
!!  Contains type definitions for phystest variables and physics-related variables

module phystest_vardefs

use machine, only: kind_phys

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! The following definition sets up the variables for use within phystest
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! Filter with CPP for PGI compiler
#ifndef __PGI
!> \section arg_table_phystest_vardefs
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

#endif

   INTEGER:: ids,ide, jds,jde, kds,kde, &
             ims,ime, jms,jme, kms,kme, &
             its,ite, jts,jte, kts,kte

   REAL   :: xlv, cp
   REAL   :: EP2,SVP1,SVP2,SVP3,SVPT0
   REAL   :: rhowater

   REAL, POINTER, DIMENSION(:,:,:) ::                           &             
                                                            t , &
                                                            qv, &
                                                            qc, &
                                                            qr, &
                                                           rho, &
                                                           pii, &
                                                          dz8w, &
                                                             z

   REAL, POINTER, DIMENSION(:,:) ::                    RAINNC,  &             
                                                       RAINNCV

   REAL :: dt_in

   CHARACTER(LEN=512) :: errmsg
   INTEGER :: ERRFLG

end module phystest_vardefs
