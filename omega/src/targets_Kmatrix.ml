(* $Id: targets_Kmatrix.ml 4926 2013-12-04 12:35:06Z jr_reuter $

   Copyright (C) 1999-2014 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
       with contributions from
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

   WHIZARD is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   WHIZARD is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

let rcs_file = RCS.parse "Targets_Kmatrix" ["K-Matrix Support routines"]
    { RCS.revision = "$Revision: 4926 $";
      RCS.date = "$Date: 2013-12-04 13:35:06 +0100 (Wed, 04 Dec 2013) $";
      RCS.author = "$Author: jr_reuter $";
      RCS.source
        = "$URL: svn+ssh://login.hepforge.org/hepforge/svn/whizard/trunk/src/omega/src/targets_Kmatrix.ml $" }

module Fortran =
  struct

    open Format

    let nl = print_newline

(* Special functions for the K matrix approach. This might be generalized
   to other functions that have to have access to the parameters and
   coupling constants. At the moment, this is hardcoded. *)

    let print pure_functions = 
      let pure =
        if pure_functions then
          "pure "
        else 
          "" in
      printf "  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"; nl ();
      printf "  !!! Special K matrix functions"; nl (); 
      printf "  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"; nl (); 
      nl();
      printf "  %sfunction width_res (z,res,w_wkm,m,g) result (w)" pure; nl ();
      printf "      real(kind=default), intent(in) :: z, w_wkm, m, g"; nl ();
      printf "      integer, intent(in) :: res"; nl ();
      printf "      real(kind=default) :: w"; nl ();
      printf "      if (z.eq.0 .AND. w_wkm.eq.0 ) then"; nl ();
      printf "        w = 0"; nl ();
      printf "      else"; nl ();
      printf "        if (w_wkm.eq.0) then"; nl ();
      printf "          select case (res)"; nl ();
      printf "            case (1) !!! Scalar isosinglet"; nl ();
      printf "              w = 3.*g**2/32./Pi * m**3/vev**2"; nl ();
      printf "            case (2) !!! Scalar isoquintet"; nl ();
      printf "              w = g**2/64./Pi * m**3/vev**2"; nl ();
      printf "            case (3) !!! Vector isotriplet"; nl ();
      printf "              w = g**2/48./Pi * m"; nl ();
      printf "            case (4) !!! Tensor isosinglet"; nl ();
      printf "              w = g**2/320./Pi * m**3/vev**2"; nl ();
      printf "            case (5) !!! Tensor isoquintet"; nl ();
      printf "              w = g**2/1920./Pi * m**3/vev**2"; nl ();
      printf "            case default"; nl ();
      printf "              w = 0"; nl ();
      printf "          end select"; nl ();
      printf "        else"; nl ();
      printf "          w = w_wkm"; nl ();
      printf "        end if"; nl ();
      printf "      end if"; nl ();
      printf "  end function width_res"; nl ();          
      nl ();
      printf "  %sfunction s0stu (s, m) result (s0)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: s0"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        s0 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        s0 = m**2 - s/2 + m**4/s * log(m**2/(s+m**2))"; nl ();
      printf "      end if"; nl ();
      printf "  end function s0stu"; nl(); 
      nl ();
      printf "  %sfunction s1stu (s, m) result (s1)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: s1"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        s1 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        s1 = 2*m**4/s + s/6 + m**4/s**2*(2*m**2+s) &"; nl();
      printf "             * log(m**2/(s+m**2))"; nl ();
      printf "      end if"; nl ();
      printf "  end function s1stu"; nl(); 
      nl ();
      printf "  %sfunction s2stu (s, m) result (s2)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: s2"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        s2 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        s2 = m**4/s**2 * (6*m**2 + 3*s) + &"; nl();
      printf "             m**4/s**3 * (6*m**4 + 6*m**2*s + s**2) &"; nl();
      printf "             * log(m**2/(s+m**2))"; nl ();
      printf "      end if"; nl ();
      printf "  end function s2stu"; nl(); 
      nl ();
      printf " !! %sfunction s3stu (s, m) result (s3)" pure; nl ();
      printf " !!     real(kind=default), intent(in) :: s, m"; nl ();
      printf " !!     real(kind=default) :: s3"; nl ();
      printf " !!     if (m.ge.1.0e08) then"; nl ();
      printf " !!       s3 = 0"; nl ();  
      printf " !!     else"; nl ();
      printf " !!       s3 = m**4/s**3 * (60*m**4 + 60*m**2*s+11*s**2) + &"; nl();
      printf " !!            m**4/s**4 *(2*m**2+s) (10*m**4 + 10*m**2*s + s**2) &"; nl();
      printf " !!            * log(m**2/(s+m**2))"; nl ();
      printf " !!     end if"; nl ();
      printf " !!  end function s3stu"; nl(); 
      nl ();
      printf "  %sfunction p0stu (s, m) result (p0)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: p0"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        p0 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        p0 = 1 + (2*s+m**2)*log(m**2/(s+m**2))/s"; nl ();
      printf "      end if"; nl (); 
      printf "  end function p0stu"; nl(); 
      nl ();
      printf "  %sfunction p1stu (s, m) result (p1)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: p1"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        p1 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        p1 = (m**2 + 2*s)/s**2 * (2*s+(2*m**2+s) &"; nl(); 
      printf "                * log(m**2/(s+m**2)))"; nl ();
      printf "      end if"; nl ();
      printf "  end function p1stu"; nl(); 
      nl ();
      printf "  %sfunction d0stu (s, m) result (d0)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: d0"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        d0 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        d0 = (2*m**2+11*s)/2 + (m**4+6*m**2*s+6*s**2) &"; nl();
      printf "              /s * log(m**2/(s+m**2))"; nl ();
      printf "      end if"; nl ();
      printf "  end function d0stu"; nl(); 
      nl ();
      printf "  %sfunction d1stu (s, m) result (d1)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s, m"; nl ();
      printf "      real(kind=default) :: d1"; nl ();
      printf "      if (m.ge.1.0e08) then"; nl ();
      printf "        d1 = 0"; nl ();  
      printf "      else"; nl ();
      printf "        d1 = (s*(12*m**4 + 72*m**2*s + 73*s**2) &"; nl();
      printf "            + 6*(2*m**2 + s)*(m**4 + 6*m**2*s + 6*s**2) &"; nl();
      printf "            * log(m**2/(s+m**2)))/6/s**2"; nl ();
      printf "      end if"; nl ();
      printf "  end function d1stu"; nl(); 
      nl ();
      printf "  %sfunction da00 (cc, s, m) result (amp_00)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: a00_0, a00_1, a00_a, a00_f"; nl ();
      printf "      complex(kind=default), dimension(1:7) :: a00"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_00"; nl ();
      printf "      real(kind=default) :: kappal, kappam, kappat"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      kappal = cc(8)*((mass(23)**2+mass(24)**2)/m(4)**2-2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappam = cc(8)*((mass(23)**4+mass(24)**4)/m(4)**2/(mass(23)**2+mass(24)**2) &"; nl (); 
      printf "                 - 2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappat = cc(8)*mass(23)**2*mass(24)**2/m(4)**4"; nl ();
      printf "      !!! Longitudinal"; nl ();
      printf "      !!! Scalar isosinglet"; nl ();
      printf "      a00(1) = -2.0 * cc(1)**2/vev**2 * s0stu(s,m(1)) "; nl ();
      printf "      if (cc(1) /= 0) then"; nl ();
      printf "        a00(1) = a00(1) - 3.0*cc(1)**2/vev**2 * &"; nl (); 
      printf "                  s**2/cmplx(s-m(1)**2,s**2/m(1)**3*width_res(w_res,1,wkm(1),m(1),cc(1)),default) "; nl ();
      printf "      end if"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      a00(2) = -5.0*cc(2)**2/vev**2 * s0stu(s,m(2)) / 3.0"; nl ();
      printf "      !!! Vector isotriplet"; nl ();
      printf "      a00(3) = -cc(3)**2*(4.0*p0stu(s,m(3)) + 6.0*s/m(3)**2)"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a00(4) = -cc(4)**2/vev**2/3 * (d0stu(s,m(4)) &"; nl ();
      printf "              - 2*kappal*s0stu(s,m(4)))"; nl ();
      printf "      if ( (cc(4) /= 0).and.(kappal /= 0)) then"; nl ();
      printf "        a00(4) = a00(4) - cc(4)**2/vev**2*kappal * &"; nl (); 
      printf "                 s**2/cmplx(s-m(4)**2,s**2/m(4)**3*10*kappal* width_res(w_res,4,wkm(4),m(4),cc(4)),default)"; nl ();
      printf "      end if"; nl ();
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      a00(5) = -5.0*cc(5)**2/vev**2*(d0stu(s,m(5)) &"; nl ();
      printf "              /3.0)/6.0"; nl ();
      printf "      !!! Transversal"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a00(6) = - cc(6)**2/Pi/vev**6*mass(23)**2*mass(24)**2/4* s**2 &"; nl ();
      printf "                * ((2-2*s/m(4)**2+s**2/m(4)**4)+kappat/2 )"; nl ();
      printf "      if (a00(6) /= 0) then"; nl ();
      printf "        a00(6) = a00(6)/cmplx(s-m(4)**2, - w_res/32/Pi * realpart(a00(6)),default) "; nl ();
      printf "      end if"; nl ();
      printf "      a00(6) = a00(6) - cc(6)**2/Pi/vev**6*mass(23)**2*mass(24)**2/12 * (s0stu(s,m(4)) &"; nl ();
      printf "               * (3*(1+2*s/m(4)**2+2*s**2/m(4)**4)+kappat ))"; nl ();
      printf "      !!! Mixed"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a00(7) = - cc(7)*cc(6)*cc(4)/Pi/vev**4*(mass(23)**2+mass(24)**2)/4 * s**2 &"; nl (); 
      printf "               *  ((1-4*s/m(4)**2+2*s**2/m(4)**4)+kappam )"; nl ();
      printf "      if (a00(7) /= 0) then"; nl ();
      printf "        a00(7) = a00(7)/cmplx(s-m(4)**2, - w_res/32/Pi * realpart(a00(7)),default) "; nl ();
      printf "      end if"; nl ();
      printf "      a00(7) = a00(7) - cc(7)*cc(6)*cc(4)/Pi/vev**4*(mass(23)**2+mass(24)**2)/12 * (s0stu(s,m(4)) &"; nl ();
      printf "              * (12*s/m(4)**2+12*s**2/m(4)**4+2*kappam ))"; nl ();
      printf "      !!! Fudge-Higgs"; nl ();
      printf "      a00_f = 2.*fudge_higgs*s/vev**2"; nl ();
      printf "      a00_f = a00_f !!! - 0*5.*(1-ghvva)**2/vev**2*mass(25)**2"; nl (); 
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a00_0 = 8.*(7.*a4 + 11.*a5)/3.*s**2/vev**4"; nl ();
      printf "      a00_1 = (25.*log(lam_reg**2/s)/9 + 11./54.0_default)*s**2/vev**4"; nl ();
      printf "      a00_a =  a00_0 !!! + a00_1/16./Pi**2"; nl ();
      printf "      !!! Unitarize"; nl ();
      printf "      if (fudge_km /= 0) then"; nl ();
      printf "        amp_00 = sum(a00)+a00_f+a00_a"; nl();
      printf "        if (amp_00 /= 0) then"; nl ();
      printf "          amp_00 = - a00_a - a00_f - part_r * (sum(a00) - a00(3)) + 1/(realpart(1/amp_00)-ii)"; nl();
      printf "        end if"; nl ();
      printf "      else"; nl ();
      printf "        amp_00 = (1-part_r) * sum(a00) + part_r * a00(3) + a00_f"; nl ();
      printf "      end if"; nl ();
      printf "      amp_00 = vev**4/s**2 * amp_00"; nl ();
      printf "  end function da00"; nl();
      nl ();
      printf "  %sfunction da02 (cc, s, m) result (amp_02)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: a02_0, a02_1, a02_a"; nl ();
      printf "      complex(kind=default), dimension(1:7) :: a02"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_02"; nl ();
      printf "      real(kind=default) :: kappal, kappam, kappat"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      kappal = cc(8)*((mass(23)**2+mass(24)**2)/m(4)**2-2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappam = cc(8)*((mass(23)**4+mass(24)**4)/m(4)**2/(mass(23)**2+mass(24)**2) &"; nl (); 
      printf "           - 2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappat = cc(8)*mass(23)**2*mass(24)**2/m(4)**4"; nl ();
      printf "      !!! Longitudinal"; nl ();
      printf "      !!! Scalar  isosinglet"; nl ();
      printf "      a02(1) = -2.0*cc(1)**2/vev**2 * s2stu(s,m(1))"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      a02(2) = -5.0*cc(2)**2/vev**2 * s2stu(s,m(2)) / 3.0"; nl ();
      printf "      !!! Vector isotriplet"; nl (); 
      printf "      a02(3) = -4.0*cc(3)**2*(2*s+m(3)**2)*s2stu(s,m(3))/m(3)**4"; nl ();
      printf "      !!! Tensor isosinglet"; nl (); 
      printf "      a02(4) = - cc(4)**2/vev**2/3 *  &"; nl ();
      printf "                  ((1.+6.*s/m(4)**2+6.*s**2/m(4)**4)-2*kappal) * s2stu(s,m(4))"; nl ();
      printf "      if (cc(4) /= 0) then"; nl ();
      printf "        a02(4) = a02(4) - cc(4)**2/vev**2/10. &"; nl (); 
      printf "                  * s**2/cmplx(s-m(4)**2,width_res(w_res,4,wkm(4),m(4),cc(4)),default)*s**2/m(4)**3"; nl ();
      printf "      end if"; nl ();
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      a02(5) = -cc(5)**2/vev**2*(5.0*(1.0+6.0* &"; nl ();
      printf "               s/m(5)**2+6.0*s**2/m(5)**4)*s2stu(s,m(5))/3.0 &"; nl ();  
      printf "               )/6.0"; nl ();
      printf "      !!! Transversal"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a02(6) = - cc(6)**2/Pi/vev**6*mass(23)**2*mass(24)**2/40* s**2"; nl ();
      printf "      if (a02(6) /= 0) then"; nl ();
      printf "        a02(6) = a02(6)/cmplx(s-m(4)**2, - w_res/32/Pi * realpart(a02(6)),default) "; nl ();
      printf "      end if"; nl ();
      printf "      a02(6) = a02(6) - cc(6)**2/Pi/vev**6*mass(23)**2*mass(24)**2/12 * (s2stu(s,m(4)) &"; nl ();
      printf "             * (3*(1+2*s/m(4)**2+2*s**2/m(4)**4)+kappat ))"; nl ();
      printf "      !!! Mixed"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a02(7) = - cc(7)*cc(6)*cc(4)/Pi/vev**4*(mass(23)**2+mass(24)**2)/20 &"; nl (); 
      printf "               * s**2"; nl ();
      printf "      if (a02(7) /= 0) then"; nl ();
      printf "        a02(7) = a02(7)/cmplx(s-m(4)**2, - w_res/32/Pi * realpart(a02(7)),default) "; nl ();
      printf "      end if"; nl ();
      printf "      a02(7) = a02(7) - cc(7)*cc(6)*cc(4)/Pi/vev**4*(mass(23)**2+mass(24)**2)/12 * (s2stu(s,m(4)) &"; nl ();
      printf "              * (12*s/m(4)**2+12*s**2/m(4)**4+2*kappam ))"; nl ();
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a02_0 = (8.*(2.*a4 + a5)/15.) *  s**2/vev**4"; nl ();
      printf "      a02_1 = (log(lam_reg**2/s)/9. - 7./135.0_default) *  s**2/vev**4"; nl ();
      printf "      a02_a = a02_0 !!! + a02_1/16/Pi**2"; nl ();
      printf "      !!! Unitarize"; nl ();
      printf "      if (fudge_km /= 0) then"; nl ();
      printf "        amp_02 = sum(a02)+a02_a"; nl();
      printf "        if (amp_02 /= 0) then"; nl ();
      printf "          amp_02 = - a02_a - part_r * (sum(a02) - a02(3)) + 1/(realpart(1/amp_02)-ii)"; nl();
      printf "        end if"; nl ();
      printf "      else"; nl ();
      printf "        amp_02 = (1-part_r) * sum(a02) + part_r * a02(3)"; nl ();
      printf "      end if"; nl ();
      printf "     amp_02 = vev**4/s**2 * amp_02"; nl ();
      printf "  end function da02"; nl(); 
      nl ();
      printf "  %sfunction da11 (cc, s, m) result (amp_11)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: a11_0, a11_1, a11_a, a11_f"; nl ();
      printf "      complex(kind=default), dimension(1:7) :: a11"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_11"; nl ();
      printf "      real(kind=default) :: kappal, kappam, kappat"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      kappal = cc(8)*((mass(23)**2+mass(24)**2)/m(4)**2-2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappam = cc(8)*((mass(23)**4+mass(24)**4)/m(4)**2/(mass(23)**2+mass(24)**2) &"; nl (); 
      printf "               - 2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappat = cc(8)*mass(23)**2*mass(24)**2/m(4)**4"; nl ();
      printf "      !!! Longitudinal"; nl ();
      printf "      !!! Scalar isosinglet"; nl ();
      printf "      a11(1) = - 2.0*cc(1)**2/vev**2 * s1stu(s,m(1))"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      a11(2) = 5.0*cc(2)**2/vev**2 * s1stu(s,m(2)) / 6.0"; nl ();
      printf "      !!! Vector isotriplet"; nl ();
      printf "      a11(3) = - cc(3)**2 * &"; nl ();
      printf "                 (s/m(3)**2 + 2. * p1stu(s,m(3)))"; nl ();
      printf "      if (cc(3) /= 0) then"; nl ();
      printf "        a11(3) = a11(3) -2./3. * cc(3)**2 * &"; nl (); 
      printf "                 s/cmplx(s-m(3)**2,s/m(3)*width_res(w_res,3,wkm(3),m(3),cc(3)),default) "; nl ();
      printf "      end if"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a11(4) = - cc(4)**2/vev**2*(d1stu(s,m(4)-2*kappal*s1stu(s,m(4))) &"; nl ();
      printf "               /3.0)"; nl ();
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      a11(5) =  5.0*cc(5)**2/vev**2*(d1stu(s,m(5)) &"; nl ();
      printf "               )/36.0"; nl ();
      printf "      !!! Transversal"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a11(6) = -cc(6)**2/Pi/vev**6*mass(23)**2*mass(24)**2/12 * (s1stu(s,m(4)) * &"; nl ();
      printf "             (3*(1+2*s/m(4)**2+2*s**2/m(4)**4)+kappat ) - (s/m(4)**2+s**2/m(4)**4)*s)"; nl ();
      printf "      !!! Mixed"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a11(7) = -cc(7)*cc(6)*cc(4)/Pi/vev**4*(mass(23)**2+mass(24)**2)/12 * (s1stu(s,m(4)) &"; nl ();
      printf "               * (12*s/m(4)**2+12*s**2/m(4)**4+2*kappam ) - 2*(s/m(4)**2+s**2/m(4)**4)*s)"; nl ();
      printf "      !!! Fudge-Higgs"; nl ();
      printf "      a11_f = fudge_higgs*s/3./vev**2"; nl ();
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a11_0 = 4.*(a4 - 2*a5)/3. * s**2/vev**4 "; nl ();
      printf "      a11_1 = - 1.0/54.0_default * s**2/vev**4"; nl (); 
      printf "      a11_a = a11_0 !!! + a11_1/16/Pi**2"; nl ();
      printf "      !!! Unitarize"; nl ();
      printf "      if (fudge_km /= 0) then"; nl ();
      printf "        amp_11 = sum(a11)+a11_f+a11_a"; nl();
      printf "        if (amp_11 /= 0) then"; nl ();
      printf "          amp_11 = - a11_a - part_r * (sum(a11) - a11(3)) + 1/(realpart(1/amp_11)-ii)"; nl();
      printf "        end if"; nl ();
      printf "      else"; nl ();
      printf "        amp_11 = (1-part_r) * sum(a11) + part_r * a11(3) + a11_f"; nl ();
      printf "      end if"; nl ();
      printf "     amp_11 = vev**4/s**2 * amp_11"; nl ();
      printf "  end function da11"; nl(); 
      nl ();
      printf "  %sfunction da20 (cc, s, m) result (amp_20)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: a20_0, a20_1, a20_a, a20_f"; nl ();
      printf "      complex(kind=default), dimension(1:7) :: a20"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_20"; nl ();
      printf "      real(kind=default) :: kappal, kappam, kappat"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      !!! Scalar isosinglet"; nl ();
      printf "      kappal = cc(8)*((mass(23)**2+mass(24)**2)/m(4)**2-2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappam = cc(8)*((mass(23)**4+mass(24)**4)/m(4)**2/(mass(23)**2+mass(24)**2) &"; nl (); 
      printf "               - 2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappat = cc(8)*mass(23)**2*mass(24)**2/m(4)**4"; nl ();
      printf "      !!! Longitudinal"; nl ();
      printf "      a20(1) = -2.0*cc(1)**2/vev**2 * s0stu(s,m(1))"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      a20(2) = - cc(2)**2/vev**2/6. * s0stu(s,m(2))"; nl ();
      printf "      if (cc(2) /= 0) then"; nl ();
      printf "        a20(2) = a20(2) - cc(2)**2/vev**2/2. *&"; nl (); 
      printf "                 s**2/cmplx(s-m(2)**2,s**2/m(2)**3*width_res(w_res,2,wkm(2),m(2),cc(2)),default)"; nl ();
      printf "      end if"; nl ();
      printf "      !!! Vector isotriplet"; nl ();
      printf "      a20(3) = cc(3)**2*(2.0*p0stu(s,m(3)) + 3.0*s/m(3)**2)"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a20(4) = - cc(4)**2/vev**2*(d0stu(s,m(4)-2*kappal*s0stu(s,m(4))) &"; nl ();
      printf "               /3.0)"; nl (); 
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      a20(5) = - cc(5)**2/vev**2*(d0stu(s,m(5)) &"; nl ();
      printf "                )/36.0"; nl ();
      printf "      !!! Transversal"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a20(6) = -cc(6)**2/Pi/vev**6*mass(23)**2*mass(24)**2/12 * (s0stu(s,m(4)) &"; nl ();
      printf "             * (3*(1+2*s/m(4)**2+2*s**2/m(4)**4)+kappat ) - 3*(s/m(4)**2-s**2/m(4)**4)*s)"; nl ();
      printf "      !!! Mixed"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a20(7) = -cc(7)*cc(6)*cc(4)/Pi/vev**4*(mass(23)**2+mass(24)**2)/12 * (s0stu(s,m(4)) &"; nl ();
      printf "               * (12*s/m(4)**2+12*s**2/m(4)**4+2*kappam ) - 6*(s/m(4)**2-s**2/m(4)**4)*s)"; nl ();
      printf "      !!! Fudge-Higgs"; nl ();
      printf "      a20_f = - fudge_higgs*s/vev**2"; nl ();
      printf "      a20_f = a20_f - 0*2*(1-ghvva)**2/vev**2*mass(25)**2"; nl (); 
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a20_0 =  16*(2*a4 + a5)/3*s**2/vev**4"; nl ();
      printf "      a20_1 = (10*log(lam_reg**2/s)/9 + 25/108.0_default) * s**2/vev**4"; nl ();
      printf "      a20_a = a20_0 !!! + a20_1/16/Pi**2"; nl ();
      printf "      !!! Unitarize"; nl ();
      printf "      if (fudge_km /= 0) then"; nl ();
      printf "        amp_20 = sum(a20)+a20_f+a20_a"; nl();
      printf "        if (amp_20 /= 0) then"; nl ();
      printf "          amp_20 = - a20_a - a20_f - part_r * (sum(a20) - a20(3)) + 1/(realpart(1/amp_20)-ii)"; nl();
      printf "        end if"; nl ();
      printf "      else"; nl ();
      printf "        amp_20 = (1-part_r) * sum(a20) + part_r * a20(3) + a20_f"; nl ();
      printf "      end if"; nl ();
      printf "     amp_20 = vev**4/s**2 * amp_20"; nl ();
      printf "  end function da20"; nl(); 
      nl ();
      printf "  %sfunction da22 (cc, s, m) result (amp_22)" pure; nl ();
      printf "      real(kind=default), intent(in) :: s"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: a22_0, a22_1, a22_a, a22_r"; nl ();
      printf "      complex(kind=default), dimension(1:7) :: a22"; nl ();
      printf "      complex(kind=default) :: ii, jj, amp_22"; nl ();
      printf "      real(kind=default) :: kappal, kappam, kappat"; nl ();
      printf "      ii = cmplx(0.0,1.0/32.0/Pi,default)"; nl ();
      printf "      jj = s**2/vev**4*ii"; nl ();
      printf "      kappal = cc(8)*((mass(23)**2+mass(24)**2)/m(4)**2-2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappam = cc(8)*((mass(23)**4+mass(24)**4)/m(4)**2/(mass(23)**2+mass(24)**2) &"; nl (); 
      printf "                 - 2*mass(23)**2*mass(24)**2/m(4)**4)"; nl ();
      printf "      kappat = cc(8)*mass(23)**2*mass(24)**2/m(4)**4"; nl ();
      printf "      !!! Longitudinal"; nl ();
      printf "      !!! Scalar isosinglet"; nl ();
      printf "      a22(1) = - 2.0*cc(1)**2/vev**2 * s2stu(s,m(1))"; nl ();
      printf "      !!! Scalar isoquintet"; nl ();
      printf "      a22(2) = - cc(2)**2/vev**2 * s2stu(s,m(2)) / 6.0"; nl ();
      printf "      !!! Vector triplet"; nl ();
      printf "      a22(3) = 2.0*cc(3)**2*(2*s+m(3)**2)*s2stu(s,m(3))/m(3)**4"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a22(4) = - cc(4)**2/vev**2*((1.0 + 6.0*s/m(4)**2 &"; nl ();
      printf "            +6.0*s**2/m(4)**4-2*kappal)*s2stu(s,m(4))/3.0)"; nl ();
      printf "      !!! Tensor isoquintet"; nl ();
      printf "      a22(5) = - cc(5)**2/vev**2/36. * &"; nl ();
      printf "                 ((1.+6.*s/m(5)**2+6.*s**2/m(5)**4 ) &"; nl (); 
      printf "                 * s2stu(s,m(5)))"; nl ();
      printf "      if (cc(5) /= 0) then"; nl ();
      printf "        a22(5) = a22(5) - cc(5)**2/vev**2/60 * &"; nl (); 
      printf "               s**2/cmplx(s-m(5)**2,s**2/m(5)**3*width_res(w_res,5,wkm(5),m(5),cc(5)),default)"; nl ();
      printf "      end if"; nl ();
      printf "      !!! Transversal"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a22(6) = -cc(6)**2/Pi/vev**6*mass(23)**2*mass(24)**2/12 * (s2stu(s,m(4)) &"; nl ();
      printf "              * (3*(1+2*s/m(4)**2+2*s**2/m(4)**4)+kappat ))"; nl ();
      printf "      !!! Mixed"; nl ();
      printf "      !!! Tensor isosinglet"; nl ();
      printf "      a22(7) = -cc(7)*cc(6)*cc(4)/Pi/vev**4*(mass(23)**2+mass(24)**2)/12 * (s2stu(s,m(4)) &"; nl ();
      printf "              * (12*s/m(4)**2+12*s**2/m(4)**4+2*kappam ))"; nl ();
      printf "      !!! Low energy theory alphas"; nl ();
      printf "      a22_0 = 4*(a4 + 2*a5)/15*s**2/vev**4 "; nl ();
      printf "      a22_1 = (2*log(lam_reg**2/s)/45 - 247/5400.0_default)*s**2/vev**4"; nl ();
      printf "      a22_a = a22_0 !!! + a22_1/16/Pi**2"; nl ();
      printf "      !!! Unitarize"; nl ();
      printf "      if (fudge_km /= 0) then"; nl ();
      printf "        amp_22 = sum(a22)+a22_a"; nl();
      printf "        if (amp_22 /= 0) then"; nl ();
      printf "          amp_22 = - a22_a - part_r * (sum(a22) - a22(3)) + 1/(realpart(1/amp_22)-ii)"; nl();
      printf "        end if"; nl ();
      printf "      else"; nl ();
      printf "        amp_22 = (1-part_r) * sum(a22) + part_r * a22(3)"; nl ();
      printf "      end if"; nl ();
      printf "     amp_22 = vev**4/s**2 * amp_22"; nl ();
      printf "  end function da22"; nl();
      nl ();
      printf "  %sfunction dalzz0_s (cc,m,k) result (alzz0_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alzz0_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz0_s = 2*g**4/costhw**2*((da00(cc,s,m) &"; nl ();
      printf "                - da20(cc,s,m))/24 &"; nl ();
      printf "                - (5.)*(da02(cc,s,m) - da22(cc,s,m))/12)"; nl ();
      printf "  end function dalzz0_s"; nl ();
      nl ();
      printf "  %sfunction dalzz0_t (cc,m,k) result (alzz0_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alzz0_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz0_t = (5.)*g**4/costhw**2*(da02(cc,s,m) - &"; nl ();
      printf "                da22(cc,s,m))/4"; nl (); 
      printf "  end function dalzz0_t"; nl ();
      nl ();
      printf "  %sfunction dalzz1_s (cc,m,k) result (alzz1_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alzz1_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz1_s = g**4/costhw**2*(da20(cc,s,m)/8 &"; nl ();
      printf "                - (5.)*da22(cc,s,m)/4)"; nl (); 
      printf "  end function dalzz1_s"; nl ();
      nl ();  
      printf "  %sfunction dalzz1_t (cc,m,k) result (alzz1_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alzz1_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz1_t = g**4/costhw**2*(- (3.)*da11(cc,s,m)/8 &"; nl ();
      printf "                + 3*(5.)*da22(cc,s,m)/8)"; nl (); 
      printf "  end function dalzz1_t"; nl ();
      nl ();  
      printf "  %sfunction dalzz1_u (cc,m,k) result (alzz1_u)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alzz1_u"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alzz1_u = g**4/costhw**2*((3.)*da11(cc,s,m)/8 &"; nl ();
      printf "                + 3*(5.)*da22(cc,s,m)/8)"; nl (); 
      printf "  end function dalzz1_u"; nl ();
      nl ();  
      printf "  %sfunction dalww0_s (cc,m,k) result (alww0_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alww0_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww0_s = g**4*((2*da00(cc,s,m) + da20(cc,s,m))/24 &"; nl ();
      printf "                  - (5.)*(2*da02(cc,s,m) + da22(cc,s,m))/12)"; nl (); 
      printf "  end function dalww0_s"; nl ();
      nl ();  
      printf "  %sfunction dalww0_t (cc,m,k) result (alww0_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alww0_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww0_t = g**4*(2*(5.)*da02(cc,s,m) - (3.)*da11(cc,s,m) &"; nl ();
      printf "                + (5.)*da22(cc,s,m))/8"; nl (); 
      printf "  end function dalww0_t"; nl ();
      nl ();  
      printf "  %sfunction dalww0_u (cc,m,k) result (alww0_u)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alww0_u"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww0_u = g**4*(2*(5.)*da02(cc,s,m) + (3.)*da11(cc,s,m) &"; nl (); 
      printf "                + (5.)*da22(cc,s,m))/8"; nl (); 
      printf "  end function dalww0_u"; nl ();
      nl ();
      printf "  %sfunction dalww2_s (cc,m,k) result (alww2_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alww2_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww2_s = g**4*(da20(cc,s,m) - 2*(5.)*da22(cc,s,m))/4 "; nl (); 
      printf "  end function dalww2_s"; nl ();
      nl ();
      printf "  %sfunction dalww2_t (cc,m,k) result (alww2_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alww2_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alww2_t = 3*(5.)*g**4*da22(cc,s,m)/4"; nl (); 
      printf "  end function dalww2_t"; nl ();
      nl ();
      printf "  %sfunction dalz4_s (cc,m,k) result (alz4_s)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alz4_s"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alz4_s = g**4/costhw**4*((da00(cc,s,m) &"; nl ();
      printf "               + 2*da20(cc,s,m))/12 &"; nl (); 
      printf "               - (5.)*(da02(cc,s,m)+2*da22(cc,s,m))/6)"; nl (); 
      printf "  end function dalz4_s"; nl ();
      nl ();
      printf "  @[<5>"; 
      printf "  %sfunction dalz4_t (cc,m,k) result (alz4_t)" pure; nl ();
      printf "      type(momentum), intent(in) :: k"; nl ();
      printf "      real(kind=default), dimension(1:8), intent(in) :: cc"; nl ();
      printf "      real(kind=default), dimension(1:5), intent(in) :: m"; nl ();
      printf "      complex(kind=default) :: alz4_t"; nl ();
      printf "      real(kind=default) :: s"; nl ();
      printf "      s = k*k"; nl ();
      printf "      alz4_t = g**4/costhw**4*(5.)*(da02(cc,s,m) &"; nl ();
      printf "               + 2*da22(cc,s,m))/4"; nl (); 
      printf "  end function dalz4_t"; nl ();
      nl ();
  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
