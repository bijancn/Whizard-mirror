! WHIZARD <<Version>> <<Date>>

! TOPPIK code by M. Jezabek, T. Teubner (v1.1, 1992), T. Teubner (1998)
!
! FB: -commented out numerical recipes code for hypergeometric 2F1
!      included in hypgeo.f90
!     -replaced PAUSE by PRINT statement to avoid compiler warning
!     -initialized 'idum' explicitly as real to avoid compiler warning
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c *********************************************************************
c
c Working version with all the different original potentials
c  like (p^2+q^2)/|p-q|^2, not transformed in terms of delta and 1/r^2;
c accuracy eps=1.d-3 possible (only), but should be save, 13.8.'98, tt.
c
c *********************************************************************
c
      subroutine tttoppik(xenergy,xtm,xtg,xalphas,xscale,xcutn,xcutv,
     u     xc0,xc1,xc2,xcdeltc,xcdeltl,xcfullc,xcfulll,xcrm2,
     u     xkincm,xkinca,jknflg,jgcflg,
     u     xkincv,jvflg,xim,xdi,np,xpp,xww,xdsdp,zvfct)
c
c *********************************************************************
c
c !! THIS IS NOT A PUBLIC VERSION !!
c
c -- Calculation of the Green function in momentum space by solving the
c     Lippmann-Schwinger equation
c     G(p) = G_0(p) + G_0(p) int_0^xcutn V(p,q) G(q) dq
c
c -- Written by Thomas Teubner, Hamburg, November 1998
c     * Based on TOPPIK Version 1.1
c        from M. Jezabek and TT, Karlsruhe, June 1992
c     * Version originally for non-constant top-width
c     * Constant width supplied here
c     * No generator included
c
c -- Use of double precision everywhere
c
c -- All masses, momenta, energies, widths in GeV
c
c -- Input parameters:
c
c    xenergy  :  E=Sqrt[s]-2*topmass
c    xtm      :  topmass (in the Pole scheme)
c    xtg      :  top-width
c    xalphas  :  alpha_s^{MSbar,n_f=5}(xscale)
c    xscale   :  soft scale  mu_{soft}
c    xcutn    :  numerical UV cutoff on all momenta
c                (UV cutoff of the Gauss-Legendre grid)
c    xcutv    :  renormalization cutoff on the
c                 delta-, the (p^2+q^2)/(p-q)^2-, and the
c                  1/r^2-[1/|p-q|]-potential:
c                 if (max(p,q).ge.xcutv) then the three potentials
c                  are set to zero in the Lippmann-Schwinger equation
c    xc0      :  0th order coefficient for the Coulomb potential,
c                 see calling example above
c    xc1      :  1st order coefficient for the Coulomb potential
c    xc2      :  2nd order coefficient for the Coulomb potential
c    xcdeltc  :  constant of the delta(r)-
c                 [= constant in momentum space-] potential
c    xcdeltl  :  constant for the additional log(q^2/mu^2)-part of the
c                 delta-potential:
c                  xcdeltc*1 + xcdeltl*log(q^2/mu^2)
c    xcfullc  :  constant of the (p^2+q^2)/(p-q)^2-potential
c    xcfulll  :  constant for the additional log(q^2/mu^2)-part of the
c                 (p^2+q^2)/(p-q)^2-potential
c    xcrm2    :  constant of the 1/r^2-[1/|p-q|]-potential
c    xkincm   :  } kinetic corrections in the 0th order Green-function:
c    xkinca   :  }  G_0(p):=1/[E+iGamma_t-p^2/m_t]*(1+xkincm)+xkinca
c     !!! WATCH THE SIGN IN G_0 !!!
c    jknflg   :  flag for these kinetic corrections:
c                 0 : no kinetic corrections applied
c                 1 : kinetic corrections applied with cutoff xcutv
c                      for  xkinca  only
c                 2 : kinetic corrections applied with cutoff xcutv
c                      for  xkinca  AND  xkincm
c    jgcflg   :  flag for G_0(p) in the LS equation:
c                 0 (standard choice) : G_0(p) as given above
c                 1 (for TIPT)        : G_0(p) = G_c^{0}(p) the 0th
c                                        order Coulomb-Green-function
c                                        in analytical form; not for
c                                        momenta  p > 1000*topmass
c    xkincv   :  additional kinematic vertexcorrection in G_0, see below:
c    jvflg    :  flag for the additional vertexcorrection  xkincv  in the
c                 ``zeroth order'' G_0(p) in the LS-equation:
c                 0 : no correction, means  G = G_0 + G_0 int V G
c                      with G_0=1/[E+iGamma_t-p^2/m_t]*(1+xkincm)+xkinca
c                 1 : apply the correction in the LS equation as
c                      G = G_0 + xkincv*p^2/m_t^2/[E+iGamma_t-p^2/m_t] + 
c                          G_0 int V G
c                     and correct the integral over Im[G(p)] to get sigma_tot
c                     from the optical theorem by the same factor.
c                     The cutoff  xcutv  is applied for these corrections.
c
c -- Output:
c
c    xim      :  R_{ttbar} from the imaginary part of the green
c                 function
c    xdi      :  R_{ttbar} form the integral over the momentum
c                 distribution (no cutoff but the numerical one here!!)
c    np       :  number of points used for the grid; fixed in tttoppik
c    xpp      :  1-dim array (max. 400 elements) giving the momenta of
c                 the Gauss-Legendre grid (pp(i) in the code)
c    xww      :  1-dim array (max. 400 elements) giving the corresponding
c                 Gauss-Legendre weights for the grid
c    xdsdp    :  1-dim array (max. 400 elements) giving the
c                 momentum distribution of top: d\sigma/dp,
c                  normalized to R,
c                  at the momenta of the Gauss-Legendre grid xpp(i)
c    zvfct    :  1-dim array (max. 400 elements) of COMPLEX*16 numbers
c                 giving the vertex function K(p), G(p)=K(p)*G_0(p)
c                 at the momenta of the grid
c
c *********************************************************************
c
c
      implicit none
      real*8
     u        pi,energy,vzero,eps,
     u        pp,
     u        tmass,tgamma,zmass,alphas,alamb5,
     u        wmass,wgamma,bmass,GFERMI,
     u        xx,critp,consde,
     u        w1,w2,sig1,sig2,const,
     u        gtpcor,etot,
     u        xenergy,xtm,xtg,xalphas,xscale,xc0,xc1,xc2,xim,xdi,
     u        xdsdp,xpp,xww,
     u        cplas,scale,c0,c1,c2,cdeltc,cdeltl,cfullc,cfulll,crm2,
     u        xcutn,dcut,xcutv,
     u        xp,xpmax,
     u        kincom,kincoa,kincov,xkincm,xkinca,xkincv,
     u        xcdeltc,xcdeltl,xcfullc,xcfulll,xcrm2
      complex*16 bb,gg,a1,a,g0,g0c,zvfct
      integer i,n,nmax,npot,np,gcflg,kinflg,jknflg,jgcflg,
     u             jvflg,vflag
      parameter (nmax=400)
      dimension pp(nmax), bb(nmax), xx(nmax), gg(nmax),
     u               w1(nmax), w2(nmax), a1(nmax),
     u               xdsdp(nmax),xpp(nmax),xww(nmax),zvfct(nmax)
      do i=1, nmax
        xpp(i)   = real(i)
        zvfct(i) = 1.
      enddo
      end
