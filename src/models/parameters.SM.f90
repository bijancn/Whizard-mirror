! $Id: parameters.SM.f90,v 1.4 2006/06/16 13:31:48 kilian Exp $
!
! Copyright (C) 1999-2017 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     Fabian Bach <fabian.bach@t-online.de>
!     Bijan Chokoufe <bijan.chokoufe@desy.de>
!     Christian Speckner <cnspeckn@googlemail.com> 
!     Christian Weiss <christian.weiss@desy.de>
!     and Hans-Werner Boschmann, Felix Braam, 
!     Sebastian Schmidt, So-young Shim, Daniel Wiesler
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
module parameters_sm
  use kinds
  use constants 
  use sm_physics !NODEP!
  implicit none
  private

  real(default), dimension(27), public :: mass, width
  real(default), public :: as
  complex(default), public :: gs, igs

  real(default), public :: e, g
  real(default), public :: sinthw, costhw, sin2thw, tanthw
  real(default), public :: qelep, qeup, qedwn
  complex(default), public :: qlep, qup, qdwn, gcc, qw, &
       gzww, gwww, ghww, ghhww, ghzz, ghhzz, &
       ghbb, ghtt, ghcc, ghtautau, gh3, gh4, ghmm, & 
       iqw, igzww, igwww, gw4, gzzww, gazww, gaaww
  real(default), public :: vev
  complex(default), dimension(2), public :: &
       gncneu, gnclep, gncup, gncdwn

  public :: import_from_whizard, model_update_alpha_s

contains

  subroutine import_from_whizard (par_array, scheme)
    real(default), dimension(25), intent(in) :: par_array
    integer, intent(in) :: scheme
    integer, parameter :: scheme_default = 1
    integer, parameter :: scheme_gf_mw_mz = 2
    integer, parameter :: scheme_cms = 3
    integer, parameter :: scheme_complex_mass_scheme = 4
    complex(default), dimension(27) :: cmass2, cmass
    complex(default) :: csin2thw, csinthw, ccos2thw, ccosthw
    integer :: i
    type :: parameter_set
       real(default) :: gf
       real(default) :: mZ
       real(default) :: mW
       real(default) :: mH
       real(default) :: alphas
       real(default) :: me
       real(default) :: mmu
       real(default) :: mtau
       real(default) :: ms
       real(default) :: mc
       real(default) :: mb
       real(default) :: mtop
       real(default) :: wtop
       real(default) :: wZ
       real(default) :: wW
       real(default) :: wH
       real(default) :: khgaz
       real(default) :: khgaga
       real(default) :: khgg
       real(default) :: xi0
       real(default) :: xipm
       real(default) :: v
       real(default) :: cw
       real(default) :: sw
       real(default) :: ee
    end type parameter_set
    type(parameter_set) :: par
    par%gf     = par_array(1)
    par%mZ     = par_array(2)
    par%mW     = par_array(3)
    par%mH     = par_array(4)
    par%alphas = par_array(5)
    par%me     = par_array(6)
    par%mmu    = par_array(7)
    par%mtau   = par_array(8)
    par%ms     = par_array(9)
    par%mc     = par_array(10)
    par%mb     = par_array(11)
    par%mtop   = par_array(12)
    par%wtop   = par_array(13)
    par%wZ     = par_array(14)
    par%wW     = par_array(15)
    par%wH     = par_array(16)
    par%khgaz  = par_array(17)
    par%khgaga = par_array(18)
    par%khgg   = par_array(19)
    par%xi0    = par_array(20)
    par%xipm   = par_array(21)
    par%v      = par_array(22)
    par%cw     = par_array(23)
    par%sw     = par_array(24)
    par%ee     = par_array(25)
    mass(1:27) = 0
    width(1:27) = 0
    mass(3) = par%ms
    mass(4) = par%mc
    mass(5) = par%mb
    mass(6) = par%mtop
    width(6) = par%wtop
    mass(11) = par%me
    mass(13) = par%mmu
    mass(15) = par%mtau
    mass(23) = par%mZ
    width(23) = par%wZ
    mass(24) = par%mW
    width(24) = par%wW
    mass(25) = par%mH
    width(25) = par%wH
    mass(26) =  par%xi0 * mass(23)
    width(26) =  0
    mass(27) =  par%xipm * mass(24)
    width(27) =  0
    do i = 1, 27
       cmass2(i) = cmplx (mass(i)**2, - mass(i)  * width(i))
       cmass(i) = sqrt (cmass2(i))
    end do
    vev = par%v
    e = par%ee
    sinthw = par%sw
    sin2thw = par%sw**2
    costhw = par%cw
    tanthw = sinthw/costhw
    ccos2thw = cmass2(24) / cmass2(23)
    csin2thw = one - ccos2thw
    ccosthw = sqrt (ccos2thw)
    csinthw = sqrt (csin2thw)
    qelep = - 1
    qeup = 2.0_default / 3.0_default
    qedwn = - 1.0_default / 3.0_default
    g  = e / sinthw
    select case (scheme)
    case (scheme_default, scheme_gf_mw_mz)
       gcc = - g / 2 / sqrt (2.0_default)
       gncneu(1) = - g / 2 / costhw * ( + 0.5_default)
       gnclep(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qelep * sin2thw)
       gncup(1)  = - g / 2 / costhw * ( + 0.5_default - 2 * qeup  * sin2thw)
       gncdwn(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qedwn * sin2thw)
       gncneu(2) = - g / 2 / costhw * ( + 0.5_default)
       gnclep(2) = - g / 2 / costhw * ( - 0.5_default)
       gncup(2)  = - g / 2 / costhw * ( + 0.5_default)
       gncdwn(2) = - g / 2 / costhw * ( - 0.5_default)
    case (scheme_cms, scheme_complex_mass_scheme)
       gcc = - e / 2 / sqrt (2.0_default) / csinthw
       gncneu(1) = - e / 2 / csinthw / ccosthw * ( + 0.5_default)
       gnclep(1) = - e / 2 / csinthw / ccosthw * ( - 0.5_default - 2 * qelep * csin2thw)
       gncup(1)  = - e / 2 / csinthw / ccosthw * ( + 0.5_default - 2 * qeup  * csin2thw)
       gncdwn(1) = - e / 2 / csinthw / ccosthw * ( - 0.5_default - 2 * qedwn * csin2thw)
       gncneu(2) = - e / 2 / csinthw / ccosthw * ( + 0.5_default)
       gnclep(2) = - e / 2 / csinthw / ccosthw * ( - 0.5_default)
       gncup(2)  = - e / 2 / csinthw / ccosthw * ( + 0.5_default)
       gncdwn(2) = - e / 2 / csinthw / ccosthw * ( - 0.5_default)
    end select
    qlep = - e * qelep
    qup = - e * qeup
    qdwn = - e * qedwn
    qw = e
    iqw = imago * qw
    select case (scheme)
    case (scheme_default, scheme_gf_mw_mz)
       gzww = g * costhw
       gwww = g
       ghww = mass(24) * g
       ghzz = mass(23) * g / costhw
       gh3 = - 3 * mass(25)**2 / vev
       gh4 = - 3 * mass(25)**2 / vev**2
       ghhww = g**2 / 2.0_default
       ghhzz = g**2 / 2.0_default / costhw**2
    case (scheme_cms, scheme_complex_mass_scheme)
       gzww = e * ccosthw / csinthw
       gwww = e / csinthw
       ghww = e * cmass(24) / csinthw
       ghzz = e * cmass(24) / csinthw / ccos2thw
       gh3 = - 3 * e * cmass2(25) / 2 / cmass(24) / csinthw
       gh4 = - 3 * e**2 * cmass2(25) / 4 / cmass2(24) / csin2thw
       ghhww = e**2 / 2.0_default / csin2thw
       ghhzz = e**2 / 2.0_default / csin2thw / ccos2thw
    end select
    igzww = imago * gzww
    igwww = imago * gwww
    gw4 = gwww**2
    gzzww = gzww**2
    gazww = gzww * qw
    gaaww = qw**2
    select case (scheme)
    case (scheme_default, scheme_gf_mw_mz)
       ghtt = - mass(6) / vev
       ghbb = - mass(5) / vev
       ghcc = - mass(4) / vev
       ghtautau = - mass(15) / vev
       ghmm = - mass(13) / vev
    case (scheme_cms, scheme_complex_mass_scheme)
       ghtt = - e * cmass(6) / 2 / cmass(24) / csinthw
       ghbb = - e * cmass(5) / 2 / cmass(24) / csinthw
       ghcc = - e * cmass(4) / 2 / cmass(24) / csinthw
       ghtautau = - e * cmass(15) / 2 / cmass(24) / csinthw
       ghmm = - e * cmass(13) / 2 / mass(24) / csinthw
    end select
    !!! Color flow basis, divide by sqrt(2)
    gs = sqrt(2.0_default*PI*par%alphas)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs    
  end subroutine import_from_whizard

  subroutine model_update_alpha_s (alpha_s)
    real(default), intent(in) :: alpha_s
    gs = sqrt(2.0_default*PI*alpha_s)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs     
  end subroutine model_update_alpha_s
end module parameters_sm
