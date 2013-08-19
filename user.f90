!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module user_spectra
    use kinds, only: default, double, single !NODEP!
    use kinds, only: i32, i64 !NODEP!
    use lorentz
    use particles
    use diagnostics, only: msg_level, msg_message

    implicit none
    private

    public :: spectrum_single, spectrum_double

    integer(kind=default) :: ncall_spectrum=0
    integer(kind=default) :: ncall_spectrum_overflow=0

    character(len=200) :: lumi_ee_file
    character(len=200) :: lumi_eg_file
    character(len=200) :: lumi_ge_file
    character(len=200) :: lumi_gg_file
    character(len=200) :: lumi_file
    character(len=200) :: photons_beam1_file
    character(len=200) :: photons_beam2_file
    character(len=200) :: ebeam_in_file
    character(len=200) :: pbeam_in_file

    real(kind=double) :: yy_electron_peak
    real(kind=double) :: photons_beam1_factor
    real(kind=double) :: photons_beam2_factor

    integer :: ndiv_eg_lumi
    real(kind=double), dimension(:,:), pointer :: yl_eg_lumi
    real(kind=double), dimension(:,:), pointer :: yu_eg_lumi
    real(kind=double), dimension(:,:), pointer :: rho_single_eg_lumi
    real(kind=double), dimension(:,:), pointer :: rho_corr_eg_lumi
    real(kind=double), dimension(2) :: avg_energy_eg_lumi

    integer :: ndiv_ge_lumi
    real(kind=double), dimension(:,:), pointer :: yl_ge_lumi
    real(kind=double), dimension(:,:), pointer :: yu_ge_lumi
    real(kind=double), dimension(:,:), pointer :: rho_single_ge_lumi
    real(kind=double), dimension(:,:), pointer :: rho_corr_ge_lumi
    real(kind=double), dimension(2) :: avg_energy_ge_lumi

    integer :: ndiv_gg_lumi
    real(kind=double), dimension(:,:), pointer :: yl_gg_lumi
    real(kind=double), dimension(:,:), pointer :: yu_gg_lumi
    real(kind=double), dimension(:,:), pointer :: rho_single_gg_lumi
    real(kind=double), dimension(:,:), pointer :: rho_corr_gg_lumi
    real(kind=double), dimension(2) :: avg_energy_gg_lumi

    integer :: ndiv_lumi
    real(kind=double), dimension(:,:), pointer :: yl_lumi
    real(kind=double), dimension(:,:), pointer :: yu_lumi
    real(kind=double), dimension(:,:), pointer :: rho_single_lumi
    real(kind=double), dimension(:,:), pointer :: rho_corr_lumi
    real(kind=double), dimension(2) :: avg_energy_lumi

    integer :: ndiv_photons_beam1
    real(kind=double), dimension(:), pointer :: yl_photons_beam1
    real(kind=double), dimension(:), pointer :: yu_photons_beam1
    real(kind=double), dimension(:), pointer :: rho_single_photons_beam1
    real(kind=double) :: avg_energy_photons_beam1

    integer :: ndiv_photons_beam2
    real(kind=double), dimension(:), pointer :: yl_photons_beam2
    real(kind=double), dimension(:), pointer :: yu_photons_beam2
    real(kind=double), dimension(:), pointer :: rho_single_photons_beam2
    real(kind=double) :: avg_energy_photons_beam2

    integer :: ndiv_ebeam_in
    real(kind=double), dimension(:), pointer :: yl_ebeam_in
    real(kind=double), dimension(:), pointer :: yu_ebeam_in
    real(kind=double), dimension(:), pointer :: rho_single_ebeam_in
    real(kind=double) :: avg_energy_ebeam_in

    integer :: ndiv_pbeam_in
    real(kind=double), dimension(:), pointer :: yl_pbeam_in
    real(kind=double), dimension(:), pointer :: yu_pbeam_in
    real(kind=double), dimension(:), pointer :: rho_single_pbeam_in
    real(kind=double) :: avg_energy_pbeam_in

    logical :: old_style_lumi_linker

  contains

    subroutine spectrum_ini (mode)
    integer, intent(in) :: mode
    integer :: iu
    integer, dimension(1) :: i1
    integer  :: i
    integer  :: ios

    real(kind=double), dimension(:,:), pointer :: yl_lumi_loc
    real(kind=double), dimension(:,:), pointer :: yu_lumi_loc
    real(kind=double), dimension(:,:), pointer :: rho_single_lumi_loc
    real(kind=double), dimension(:,:), pointer :: rho_corr_lumi_loc
    real(kind=double), dimension(2) :: avg_energy_lumi_loc


    call getenv("LUMI_EE_LINKER",lumi_ee_file)
    call getenv("LUMI_EG_LINKER",lumi_eg_file)
    call getenv("LUMI_GE_LINKER",lumi_ge_file)
    call getenv("LUMI_GG_LINKER",lumi_gg_file)
    call getenv("LUMI_LINKER",lumi_file)
    call getenv("PHOTONS_B1",photons_beam1_file)
    call getenv("PHOTONS_B2",photons_beam2_file)
    call getenv("EBEAM",ebeam_in_file)
    call getenv("PBEAM",pbeam_in_file)
    lumi_file=adjustr(lumi_file)
    write(lumi_file(198:200),FMT='(i3.3)') abs(mode)
    lumi_file=adjustl(lumi_file)
    call msg_message(" lumi_file=" // lumi_file)

    lumi_ee_file=adjustr(lumi_ee_file)
    write(lumi_ee_file(198:200),FMT='(i3.3)') abs(mode)
    lumi_ee_file=adjustl(lumi_ee_file)
    call msg_message(" lumi_ee_file=" // lumi_ee_file)

    lumi_eg_file=adjustr(lumi_eg_file)
    write(lumi_eg_file(198:200),FMT='(i3.3)') abs(mode)
    lumi_eg_file=adjustl(lumi_eg_file)
    call msg_message(" lumi_eg_file=" // lumi_eg_file)

    lumi_ge_file=adjustr(lumi_ge_file)
    write(lumi_ge_file(198:200),FMT='(i3.3)') abs(mode)
    lumi_ge_file=adjustl(lumi_ge_file)
    call msg_message(" lumi_ge_file=" // lumi_ge_file)

    lumi_gg_file=adjustr(lumi_gg_file)
    write(lumi_gg_file(198:200),FMT='(i3.3)') abs(mode)
    lumi_gg_file=adjustl(lumi_gg_file)
    call msg_message(" lumi_gg_file=" // lumi_gg_file)


    iu=free_unit()
    open(unit=iu,file=lumi_file,action='read',status='old',form='unformatted',iostat=ios)
    check_old_or_new_style: if(ios.eq.0) then
       old_style_lumi_linker=.true.
    else check_old_or_new_style
       old_style_lumi_linker=.false.
       close(iu)
       open(unit=iu,file=lumi_ee_file,action='read',status='old',form='unformatted')
    end if check_old_or_new_style
    read(iu) ndiv_lumi
    allocate (rho_corr_lumi(ndiv_lumi,ndiv_lumi))
    allocate (rho_single_lumi(2,ndiv_lumi))
    allocate (yl_lumi(2,ndiv_lumi))
    allocate (yu_lumi(2,ndiv_lumi))
    allocate (rho_corr_lumi_loc(ndiv_lumi,ndiv_lumi))
    allocate (rho_single_lumi_loc(2,ndiv_lumi))
    allocate (yl_lumi_loc(2,ndiv_lumi))
    allocate (yu_lumi_loc(2,ndiv_lumi))
    read(iu) yl_lumi_loc,yu_lumi_loc,rho_single_lumi_loc,rho_corr_lumi_loc,avg_energy_lumi_loc
    close(iu)
    check_mode_5: if(abs(mode).ne.5) then
       yl_lumi=yl_lumi_loc
       yu_lumi=yu_lumi_loc
       rho_single_lumi=rho_single_lumi_loc
       rho_corr_lumi=rho_corr_lumi_loc
       avg_energy_lumi=avg_energy_lumi_loc
    else check_mode_5

       yl_lumi(1,:)=yl_lumi_loc(2,:)
       yu_lumi(1,:)=yu_lumi_loc(2,:)
       rho_single_lumi(1,:)=rho_single_lumi_loc(2,:)
       avg_energy_lumi(1)=avg_energy_lumi_loc(2)

       yl_lumi(2,:)=yl_lumi_loc(1,:)
       yu_lumi(2,:)=yu_lumi_loc(1,:)
       rho_single_lumi(2,:)=rho_single_lumi_loc(1,:)
       avg_energy_lumi(2)=avg_energy_lumi_loc(1)

       rho_corr_lumi=transpose(rho_corr_lumi_loc)

    end if check_mode_5
    i1=maxloc(rho_single_lumi(1,:))
    yy_electron_peak=0.5d0*(yl_lumi(1,i1(1))+yu_lumi(1,i1(1)))
    if ( msg_level > 3 ) then
       call msg_message(" ndiv_lumi,avg_energy_lumi=" // ndiv_lumi // " " // avg_energy_lumi)
       loop_i_lumi: do i=1,ndiv_lumi
          call msg_message(" i,yl_lumi(:,i),yu_lumi(:,i),rho_single_lumi(:,i),rho_corr_lumi(i,i)="// i // " " // yl_lumi(:,i) // yu_lumi(:,i) // rho_single_lumi(:,i) // rho_corr_lumi(i,i))
       end do loop_i_lumi
       call msg_message(" i1,yy_electron_peak=" // i1,yy_electron_peak)
    endif

    ebeam_in_file=adjustr(ebeam_in_file)
    write(ebeam_in_file(198:200),FMT='(i3.3)') abs(mode)
    ebeam_in_file=adjustl(ebeam_in_file)
    call msg_message( " ebeam_in_file=" // ebeam_in_file )
    open(unit=iu,file=ebeam_in_file,action='read',status='old',form='unformatted')
    read(iu) ndiv_ebeam_in
    allocate (rho_single_ebeam_in(ndiv_ebeam_in))
    allocate (yl_ebeam_in(ndiv_ebeam_in))
    allocate (yu_ebeam_in(ndiv_ebeam_in))
    read(iu) yl_ebeam_in,yu_ebeam_in,rho_single_ebeam_in,avg_energy_ebeam_in
    close(iu)
    call msg_message(" ndiv_ebeam_in,avg_energy_ebeam_in=" // ndiv_ebeam_in // avg_energy_ebeam_in)
    loop_i_ebeam_in: do i=1,ndiv_ebeam_in
       call msg_message(" i,yl_ebeam_in(i),yu_ebeam_in(i),rho_single_ebeam_in(i)=" // i // yl_ebeam_in(i) // yu_ebeam_in(i) // rho_single_ebeam_in(i))
    end do loop_i_ebeam_in

    check_old_or_new_style_photons: if(old_style_lumi_linker) then

       pbeam_in_file=adjustr(pbeam_in_file)
       write(pbeam_in_file(198:200),FMT='(i3.3)') abs(mode)
       pbeam_in_file=adjustl(pbeam_in_file)
       print *, " pbeam_in_file=", pbeam_in_file
       open(unit=iu,file=pbeam_in_file,action='read',status='old',form='unformatted')
       read(iu) ndiv_pbeam_in
       allocate (rho_single_pbeam_in(ndiv_pbeam_in))
       allocate (yl_pbeam_in(ndiv_pbeam_in))
       allocate (yu_pbeam_in(ndiv_pbeam_in))
       read(iu) yl_pbeam_in,yu_pbeam_in,rho_single_pbeam_in,avg_energy_pbeam_in
       close(iu)
       print *, " ndiv_pbeam_in,avg_energy_pbeam_in=", ndiv_pbeam_in,avg_energy_pbeam_in
       loop_i_pbeam_in: do i=1,ndiv_pbeam_in
          print *, " i,yl_pbeam_in(i),yu_pbeam_in(i),rho_single_pbeam_in(i)=", i,yl_pbeam_in(i),yu_pbeam_in(i),rho_single_pbeam_in(i)
       end do loop_i_pbeam_in

       photons_beam1_file=adjustr(photons_beam1_file)
       write(photons_beam1_file(198:200),FMT='(i3.3)') abs(mode)
       photons_beam1_file=adjustl(photons_beam1_file)
       print *, " photons_beam1_file=", photons_beam1_file
       open(unit=iu,file=photons_beam1_file,action='read',status='old',form='unformatted')
       read(iu) ndiv_photons_beam1
       allocate (rho_single_photons_beam1(ndiv_photons_beam1))
       allocate (yl_photons_beam1(ndiv_photons_beam1))
       allocate (yu_photons_beam1(ndiv_photons_beam1))
       read(iu) yl_photons_beam1,yu_photons_beam1,rho_single_photons_beam1,avg_energy_photons_beam1
       close(iu)
       if ( msg_level > 3 ) then
          print *, " ndiv_photons_beam1,avg_energy_photons_beam1=", ndiv_photons_beam1,avg_energy_photons_beam1
          loop_i_photons_beam1: do i=1,ndiv_photons_beam1
             print *, " i,yl_photons_beam1(i),yu_photons_beam1(i),rho_single_photons_beam1(i)=", i,yl_photons_beam1(i),yu_photons_beam1(i),rho_single_photons_beam1(i)
          end do loop_i_photons_beam1
       endif

       photons_beam2_file=adjustr(photons_beam2_file)
       write(photons_beam2_file(198:200),FMT='(i3.3)') abs(mode)
       photons_beam2_file=adjustl(photons_beam2_file)
       if ( msg_level > 0 ) print *, " photons_beam2_file=", photons_beam2_file
       open(unit=iu,file=photons_beam2_file,action='read',status='old',form='unformatted')
       read(iu) ndiv_photons_beam2
       allocate (rho_single_photons_beam2(ndiv_photons_beam2))
       allocate (yl_photons_beam2(ndiv_photons_beam2))
       allocate (yu_photons_beam2(ndiv_photons_beam2))
       read(iu) yl_photons_beam2,yu_photons_beam2,rho_single_photons_beam2,avg_energy_photons_beam2
       close(iu)
       if ( msg_level > 3 ) then
          print *, " ndiv_photons_beam2,avg_energy_photons_beam2=", ndiv_photons_beam2,avg_energy_photons_beam2
          loop_i_photons_beam2: do i=1,ndiv_photons_beam2
             print *, " i,yl_photons_beam2(i),yu_photons_beam2(i),rho_single_photons_beam2(i)=", i,yl_photons_beam2(i),yu_photons_beam2(i),rho_single_photons_beam2(i)
          end do loop_i_photons_beam2
       endif


       photons_beam1_factor=(avg_energy_ebeam_in-avg_energy_lumi(1))/avg_energy_photons_beam1
       photons_beam2_factor=(avg_energy_pbeam_in-avg_energy_lumi(2))/avg_energy_photons_beam2

       print *, " photons_beam1_factor,photons_beam2_factor=", photons_beam1_factor,photons_beam2_factor

    else check_old_or_new_style_photons

       open(unit=iu,file=lumi_eg_file,action='read',status='old',form='unformatted')
       read(iu) ndiv_eg_lumi
       allocate (rho_corr_eg_lumi(ndiv_eg_lumi,ndiv_eg_lumi))
       allocate (rho_single_eg_lumi(2,ndiv_eg_lumi))
       allocate (yl_eg_lumi(2,ndiv_eg_lumi))
       allocate (yu_eg_lumi(2,ndiv_eg_lumi))
       read(iu) yl_eg_lumi,yu_eg_lumi,rho_single_eg_lumi,rho_corr_eg_lumi,avg_energy_eg_lumi
       close(iu)

       open(unit=iu,file=lumi_ge_file,action='read',status='old',form='unformatted')
       read(iu) ndiv_ge_lumi
       allocate (rho_corr_ge_lumi(ndiv_ge_lumi,ndiv_ge_lumi))
       allocate (rho_single_ge_lumi(2,ndiv_ge_lumi))
       allocate (yl_ge_lumi(2,ndiv_ge_lumi))
       allocate (yu_ge_lumi(2,ndiv_ge_lumi))
       read(iu) yl_ge_lumi,yu_ge_lumi,rho_single_ge_lumi,rho_corr_ge_lumi,avg_energy_ge_lumi
       close(iu)

       open(unit=iu,file=lumi_gg_file,action='read',status='old',form='unformatted')
       read(iu) ndiv_gg_lumi
       allocate (rho_corr_gg_lumi(ndiv_gg_lumi,ndiv_gg_lumi))
       allocate (rho_single_gg_lumi(2,ndiv_gg_lumi))
       allocate (yl_gg_lumi(2,ndiv_gg_lumi))
       allocate (yu_gg_lumi(2,ndiv_gg_lumi))
       read(iu) yl_gg_lumi,yu_gg_lumi,rho_single_gg_lumi,rho_corr_gg_lumi,avg_energy_gg_lumi
       close(iu)

    end if check_old_or_new_style_photons


  end subroutine spectrum_ini

end module user_spectra

subroutine beam_spectrum_info (n_in, n_out, n_states, n_col, n_dim, n_var) bind(C)
  use iso_c_binding
  integer(c_int), intent(inout) :: n_in, n_out, n_states, n_col
  integer(c_int), intent(inout) :: n_dim, n_var
  n_in = 2
  n_out = 2
  n_states = 4
  n_var = 2
end subroutine beam_spectrum_info

subroutine beam_spectrum_mask (i_prt, m_flv, m_col, m_hel, i_lock) bind(C)
  use iso_c_binding
  integer(c_int), intent(in) :: i_prt
  integer(c_int), intent(inout) :: m_flv, m_hel, m_col, i_lock
  ! ... code that sets m_flv, m_hel, m_col, i_lock
  select case (i_prt)
  case (1)
     i_lock = 3
  case (2)
     i_lock = 4
  case (3)
     i_lock = 1
  case (4)
     i_lock = 2
  end select
end subroutine beam_spectrum_mask

subroutine beam_spectrum_state (i_state, i_prt, flv, hel, col) bind(C)
  use iso_c_binding
  integer(c_int), intent(in) :: i_state, i_prt
  integer(c_int), intent(inout) :: flv, hel
  integer(c_int), dimension(*), intent(inout) :: col
  ! ... code that sets flv, hel, col
end subroutine beam_spectrum_state

subroutine beam_spectrum_kinematics (prt_in, rval, prt_out, xval) bind(C)
  use iso_c_binding
  use c_particles
  type(c_prt_t), dimension(*), intent(in) :: prt_in
  real(c_double), dimension(*), intent(in) :: rval
  type(c_prt_t), dimension(*), intent(inout) :: prt_out
  real(c_double), dimension(*), intent(out) :: xval
  ! ... code that computes prt_out and xval
end subroutine beam_spectrum_kinematics

subroutine beam_spectrum_evaluate (xval, scale, fval) bind(C)
  use iso_c_binding
  real(c_double), dimension(*), intent(in) :: xval
  real(c_double), intent(in) :: scale
  real(c_double), dimension(*), intent(out) :: fval
end subroutine beam_spectrum_evaluate
