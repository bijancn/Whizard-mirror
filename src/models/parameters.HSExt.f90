! $Id: parameters.HSExt.f90,v 1.4 2006/06/16 13:31:48 kilian Exp $
!
! Copyright (C) 1999-2014 by 
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!     with contributions from
!     Christian Speckner <cnspeckn@googlemail.com>
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
module parameters_hsext
  use kinds
  use constants 
  use sm_physics !NODEP!
  implicit none
  private

  real(default), dimension(35), public :: mass, width
  real(default), public :: as
  complex(default), public :: gs, igs

  real(default), public :: e, g, e_em
  real(default), public :: sinthw, costhw, sin2thw, tanthw
  real(default), public :: qelep, qeup, qedwn
  real(default), public :: ttop, tbot, tch, ttau, tw
  real(default), public :: ltop, lbot, lc, ltau, lw
  complex(default), public :: qlep, qup, qdwn, gcc, qw, &
       gzww, gwww, ghww, gsww, ghhww, ghsww, gssww, &
       ghzz, ghhzz, gszz, ghszz, gsszz, &
       ghbb, ghtt, ghcc, ghtautau, gh3, gh4, ghhs, ghss, gsss, &
       gxgaga, gxgaz, gxgg, ghgaga, ghgaz, ghgg, gsgaga, gsgaz, gsgg, &
       ghmm, gsbb, gstt, gstautau, gscc, &
       gsmm, iqw, igzww, igwww, gw4, gzzww, gazww, gaaww
  real(default), public :: vev
  real(default) :: sina, cosa
  complex(default), dimension(2), public :: &
       gncneu, gnclep, gncup, gncdwn
  real(default), public :: xi0 = 0, xipm = 0

  public :: import_from_whizard, model_update_alpha_s

contains

  subroutine import_from_whizard (par_array)
    real(default), dimension(27), intent(in) :: par_array
    type :: parameter_set
       real(default) :: gf
       real(default) :: mZ
       real(default) :: mW
       real(default) :: mH
       real(default) :: msing
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
       real(default) :: wsing
       real(default) :: sinal
       real(default) :: tanbe
       real(default) :: khgaz
       real(default) :: khgaga
       real(default) :: khgg
       real(default) :: v
       real(default) :: cw
       real(default) :: sw
       real(default) :: ee
    end type parameter_set
    type(parameter_set) :: par
    !!! This corresponds to 1/alpha = 137.03598949333
    real(default), parameter :: &
         alpha = 1.0_default/137.03598949333_default
    e_em = sqrt(4.0_default * PI * alpha)
    par%gf     = par_array(1)
    par%mZ     = par_array(2)
    par%mW     = par_array(3)
    par%mH     = par_array(4)
    par%msing  = par_array(5)
    par%alphas = par_array(6)
    par%me     = par_array(7)
    par%mmu    = par_array(8)
    par%mtau   = par_array(9)
    par%ms     = par_array(10)
    par%mc     = par_array(11)
    par%mb     = par_array(12)
    par%mtop   = par_array(13)
    par%wtop   = par_array(14)
    par%wZ     = par_array(15)
    par%wW     = par_array(16)
    par%wH     = par_array(17)
    par%wsing  = par_array(18)
    par%sinal  = par_array(19)
    par%tanbe  = par_array(20)
    par%khgaz  = par_array(21)
    par%khgaga = par_array(22)
    par%khgg   = par_array(23)
    par%v      = par_array(24)
    par%cw     = par_array(25)
    par%sw     = par_array(26)
    par%ee     = par_array(27)
    mass(1:35) = 0
    width(1:35) = 0
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
    mass(26) =  xi0 * mass(23)
    width(26) =  0
    mass(27) =  xipm * mass(24)
    width(27) =  0
    mass(35) = par%msing
    width(35) = par%wsing
    ttop = 4.0_default * mass(6)**2 / mass(25)**2
    tbot = 4.0_default * mass(5)**2 / mass(25)**2
    tch  = 4.0_default * mass(4)**2 / mass(25)**2
    ttau = 4.0_default * mass(15)**2 / mass(25)**2
    tw   = 4.0_default * mass(24)**2 / mass(25)**2  
    ltop = 4.0_default * mass(6)**2 / mass(23)**2
    lbot = 4.0_default * mass(5)**2 / mass(23)**2  
    lc   = 4.0_default * mass(4)**2 / mass(23)**2
    ltau = 4.0_default * mass(15)**2 / mass(23)**2
    lw   = 4.0_default * mass(24)**2 / mass(23)**2
    vev = par%v
    e = par%ee
    sinthw = par%sw
    sin2thw = par%sw**2    
    costhw = par%cw
    tanthw = sinthw/costhw
    sina = par%sinal
    cosa = sqrt(1 - sina**2)
    qelep = - 1
    qeup = 2.0_default / 3.0_default
    qedwn = - 1.0_default / 3.0_default
    g = e / sinthw
    gcc = - g / 2 / sqrt (2.0_default)
    gncneu(1) = - g / 2 / costhw * ( + 0.5_default)
    gnclep(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qelep * sin2thw)
    gncup(1)  = - g / 2 / costhw * ( + 0.5_default - 2 * qeup  * sin2thw)
    gncdwn(1) = - g / 2 / costhw * ( - 0.5_default - 2 * qedwn * sin2thw)
    gncneu(2) = - g / 2 / costhw * ( + 0.5_default)
    gnclep(2) = - g / 2 / costhw * ( - 0.5_default)
    gncup(2)  = - g / 2 / costhw * ( + 0.5_default)
    gncdwn(2) = - g / 2 / costhw * ( - 0.5_default)
    qlep = - e * qelep
    qup = - e * qeup
    qdwn = - e * qedwn
    qw = e
    iqw = (0,1)*qw
    gzww = g * costhw
    igzww = (0,1)*gzww
    gwww = g
    igwww = (0,1)*gwww
    gw4 = gwww**2
    gzzww = gzww**2
    gazww = gzww * qw
    gaaww = qw**2
    ghww = mass(24) * g * cosa
    gsww = mass(24) * g * sina
    ghhww = g**2 / 2.0_default * cosa**2
    ghsww = g**2 / 2.0_default * cosa * sina 
    gssww = g**2 / 2.0_default * sina**2
    ghzz = mass(23) * g / costhw * cosa
    gszz = mass(23) * g / costhw * sina
    ghhzz = g**2 / 2.0_default / costhw**2 * cosa**2
    ghszz = g**2 / 2.0_default / costhw**2 * cosa * sina
    gsszz = g**2 / 2.0_default / costhw**2 * sina**2
    ghtt = - mass(6) / vev * cosa
    ghbb = - mass(5) / vev * cosa
    ghcc = - mass(4) / vev * cosa
    ghtautau = - mass(15) / vev * cosa
    ghmm = - mass(13) / vev * cosa
    gstt = - mass(6) / vev * sina
    gsbb = - mass(5) / vev * sina
    gscc = - mass(4) / vev * sina
    gstautau = - mass(15) / vev * sina
    gsmm = - mass(13) / vev * sina
    gh3 = - 3 * mass(25)**2 / vev * sina**3
    ghhs = - (mass(25)**2 + mass(35)**2) / (2 * vev) * &
         (sina * par%tanbe + cosa) * sina * cosa           
    !!! These couplings are not checked, and probably wrong
    ghss = ghhs
    gsss = - 3 * mass(25)**2 / vev * cosa**3
    gh4 = - 3 * mass(25)**2 / vev**2
    !!! Color flow basis, divide by sqrt(2)
    gs = sqrt(2.0_default*PI*par%alphas)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! Higgs anomaly couplings
    !!! SM LO loop factor (top,bottom,W)
    gxgaga = (-1._default) * alpha / vev / 2.0_default / PI * &
         (( 4.0_default * (fonehalf(ttop) + fonehalf(tch)) &
         + fonehalf(tbot)) / 3.0_default + fonehalf(ttau) + fone(tw)) &
         * sqrt(par%khgaga) 
    ghgaga = gxgaga * cosa
    gsgaga = gxgaga * sina
    !!! asymptotic limit:
    !!! ghgaga = (par%ee)**2 / vev / &
    !!!      9.0_default / pi**2
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! SM LO loop factor (only top and W)
    gxgaz = e * e_em / 8.0_default / PI**2 / vev * ( &
          ( - 2.0_default + &
          16.0_default/3.0_default * sin2thw) * &
          (tri_i1(ttop,ltop) - tri_i2(ttop,ltop)) / costhw & 
          + ( - 1.0_default + &
          4.0_default/3.0_default * sin2thw) & 
          * (tri_i1(tbot,lbot) - tri_i2(tbot,lbot)) / costhw &
          + (-1.0_default + 4.0_default * sin2thw) &
          * (tri_i1(ttau,ltau) - tri_i2(ttau,ltau)) / costhw &
           - costhw * ( 4.0_default * (3.0_default - tanthw**2) * &
           tri_i2(tw,lw) + ((1 + 2.0_default/tw) * tanthw**2 - ( &
           5.0_default + 2.0_default/tw)) * tri_i1(tw,lw)) &
          )/sinthw * sqrt(par%khgaz) 
    ghgaz = gxgaz * cosa
    gsgaz = gxgaz * sina
    !!! SM LO order loop factor with 
    !!! N(N)LO K factor = 2.1 (only top)
    !!! Limit of infinite top quark mass:
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! We use par%gg because of sqrt(2) above
    gxgg = (-1._double) * par%alphas / vev / 4.0_default / PI * &
         (fonehalf(ttop) + fonehalf(tbot) + fonehalf(tch)) * &
         sqrt(par%khgg) 
    ghgg = gxgg * cosa
    gsgg = gxgg * sina
    !!! ghgg   = par%alphas / 3.0_default &
    !!!      / vev / pi * 2.1_default
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end subroutine import_from_whizard

  subroutine model_update_alpha_s (alpha_s)
    real(default), intent(in) :: alpha_s
    gs = sqrt(2.0_default*PI*alpha_s)
    igs = cmplx (0.0_default, 1.0_default, kind=default) * gs     
    !!! The Hgg coupling should not get a running alpha_s
  end subroutine model_update_alpha_s
end module parameters_hsext
