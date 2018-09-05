module phystest_main

use machine, only: kind_phys

implicit none

contains

subroutine phystest_main_sub()

  ! Add the CCPP specific types and functions
  use :: ccpp_api,                           &
         only: ccpp_t,                       &
               ccpp_init,                    &
               ccpp_finalize,                &
               ccpp_physics_init,            &
               ccpp_physics_run,             &
               ccpp_physics_finalize,        &
               ccpp_field_add

! NOTE -- The variables managed by the CCPP are included in the the ccpp_modules.inc file in the "use" statements
#include "ccpp_modules.inc"

  implicit none


  integer                           :: i, j, k

  ! Create the CCPP required cdata structure
  type(ccpp_t), allocatable, target                      :: cdata(:)

  integer                                                :: ierr
  integer ,parameter :: ncols=1
  integer ,parameter :: ntimes=1

  integer :: file_unit
  character (len=80) :: file_name
  character (len=80) :: string


  file_name = 'wrf_kessler_input.txt'

  file_unit =17
  open (unit=file_unit,file=TRIM(file_name),status='old',form='formatted',access='sequential')

  READ(file_unit,*) string
  READ(file_unit,*) ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte  

  ! Allocate the host variables

  aLLocate(t      (ims:ime,kms:kme,jms:jme))
  aLLocate(qv     (ims:ime,kms:kme,jms:jme))
  aLLocate(qc     (ims:ime,kms:kme,jms:jme))
  aLLocate(qr     (ims:ime,kms:kme,jms:jme))
  aLLocate(rho    (ims:ime,kms:kme,jms:jme))
  aLLocate(pii    (ims:ime,kms:kme,jms:jme))
  aLLocate(dz8w   (ims:ime,kms:kme,jms:jme))
  aLLocate(z      (ims:ime,kms:kme,jms:jme))
  aLLocate(rainnc (ims:ime,        jms:jme))
  aLLocate(rainncv(ims:ime,        jms:jme))

  ! Allocate the list of pointers of the fields above

  allocate(cdata(ncols))

  ! Initialize the host variables
  call get_data( t, qv, qc, qr, rho, pii                  &
               ,dt_in, z, xlv, cp                        &
               ,EP2,SVP1,SVP2,SVP3,SVPT0,rhowater        &
               ,dz8w                                     &
               ,RAINNC, RAINNCV                          &
               ,ids,ide ,jds,jde ,kds,kde                & ! domain dims
               ,ims,ime ,jms,jme ,kms,kme                & ! memory dims
               ,its,ite ,jts,jte ,kts,kte)                 ! tile   dims

  do i = 1, ncols

      ! Use the suite information to setup the run
      call ccpp_init( '../suites/suite_phystest_test_simple1.xml', cdata(i), ierr)
      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_init for column ', i, '. Exiting...'
          stop
      end if

! use ccpp_fields.inc to call ccpp_field_add for all variables to be exposed to CCPP (this is auto-generated from /src/ccpp/scripts/ccpp_prebuild.py - the script parses tables in the phystest_var_defs.f90)
#include "ccpp_fields.inc"

      ! initialize each column's physics
      call ccpp_physics_init(cdata(i), ierr=ierr)
      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_init for column ', i, '. Exiting...'
          stop
      end if

  end do

  ! loop over all time steps
  do j = 1, ntimes
    do i = 1, ncols
       call ccpp_physics_run(cdata(i), ierr=ierr)
       if (ierr/=0) then
           write(*,*) errmsg
           write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_run for column ', i, '. Exiting...'
           stop
       end if

     end do

  end do


  do i=1, ncols
      call ccpp_finalize(cdata(i), ierr)
      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_finalize for column ', i, '. Exiting...'
          stop
      end if
  end do

end subroutine phystest_main_sub

end module phystest_main

!> \brief Main SCM program that calls the main SCM subroutine
!!
!! The Doxygen documentation system cannot handle in-body comments in Fortran main programs, so the "main" program was put in the
!! subroutine \ref phystest_main_sub above.
program phystest
  use phystest_main
  call phystest_main_sub()
end program phystest
