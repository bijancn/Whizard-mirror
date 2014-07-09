      module dhelas95
      contains
c
c ======================================================================
c
      subroutine boostx(p,q , pboost)
c
c this subroutine performs the lorentz boost of a four-momentum.  the   
c momentum p is assumed to be given in the rest frame of q.  pboost is  
c the momentum p boosted to the frame in which q is given.  q must be a 
c timelike momentum.                                                    
c                                                                       
c input:                                                                
c       real    p(0:3)         : four-momentum p in the q rest  frame   
c       real    q(0:3)         : four-momentum q in the boosted frame   
c                                                                       
c output:                                                               
c       real    pboost(0:3)    : four-momentum p in the boosted frame   
c
      real*8    p(0:3),q(0:3),pboost(0:3),pq,qq,m,lf
c
      real*8 r_zero
      parameter( r_zero=0.0d0 )
c
      qq=q(1)**2+q(2)**2+q(3)**2
c
      if ( qq .ne. r_zero ) then
         pq=p(1)*q(1)+p(2)*q(2)+p(3)*q(3)
         m=sqrt(q(0)**2-qq)
         lf=((q(0)-m)*pq/qq+p(0))/m
         pboost(0) = (p(0)*q(0)+pq)/m
         pboost(1) =  p(1)+q(1)*lf
         pboost(2) =  p(2)+q(2)*lf
         pboost(3) =  p(3)+q(3)*lf
      else
         pboost(0)=p(0)
         pboost(1)=p(1)
         pboost(2)=p(2)
         pboost(3)=p(3)
      endif
c
      return
      end subroutine
c
c **********************************************************************
c
      subroutine coup1x(sw2 , gw,gwwa,gwwz)
c
c this subroutine sets up the coupling constants of the gauge bosons in 
c the standard model.                                                   
c                                                                       
c input:                                                                
c       real    sw2            : square of sine of the weak angle       
c                                                                       
c output:                                                               
c       real    gw             : weak coupling constant                 
c       real    gwwa           : dimensionless coupling of w-,w+,a      
c       real    gwwz           : dimensionless coupling of w-,w+,z      
c
      real*8    sw2,gw,gwwa,gwwz,alpha,fourpi,ee,sw,cw
c
      real*8 r_one, r_four, r_ote, r_pi, r_ialph
      parameter( r_one=1.0d0, r_four=4.0d0, r_ote=128.0d0 )
      parameter( r_pi=3.14159265358979323846d0, r_ialph=137.0359895d0 )
c
      alpha = r_one / r_ote
c      alpha = r_one / r_ialph
      fourpi = r_four * r_pi
      ee=sqrt( alpha * fourpi )
      sw=sqrt( sw2 )
      cw=sqrt( r_one - sw2 )
c
      gw    =  ee/sw
      gwwa  =  ee
      gwwz  =  ee*cw/sw
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine coup2x(sw2 , gal,gau,gad,gwf,gzn,gzl,gzu,gzd,g1)
c
c this subroutine sets up the coupling constants for the fermion-       
c fermion-vector vertices in the standard model.  the array of the      
c couplings specifies the chirality of the flowing-in fermion.  g??(1)  
c denotes a left-handed coupling, and g??(2) a right-handed coupling.   
c                                                                       
c input:                                                                
c       real    sw2            : square of sine of the weak angle       
c                                                                       
c output:                                                               
c       real    gal(2)         : coupling with a of charged leptons     
c       real    gau(2)         : coupling with a of up-type quarks      
c       real    gad(2)         : coupling with a of down-type quarks    
c       real    gwf(2)         : coupling with w-,w+ of fermions        
c       real    gzn(2)         : coupling with z of neutrinos           
c       real    gzl(2)         : coupling with z of charged leptons     
c       real    gzu(2)         : coupling with z of up-type quarks      
c       real    gzd(2)         : coupling with z of down-type quarks    
c       real    g1(2)          : unit coupling of fermions              
c
      real*8 gal(2),gau(2),gad(2),gwf(2),gzn(2),gzl(2),gzu(2),gzd(2),
     &     g1(2),sw2,alpha,fourpi,ee,sw,cw,ez,ey
c
      real*8 r_zero, r_half, r_one, r_two, r_three, r_four, r_ote
      real*8 r_pi, r_ialph
      parameter( r_zero=0.0d0, r_half=0.5d0, r_one=1.0d0, r_two=2.0d0,
     $     r_three=3.0d0 )
      parameter( r_four=4.0d0, r_ote=128.0d0 )
      parameter( r_pi=3.14159265358979323846d0, r_ialph=137.0359895d0 )
c
      alpha = r_one / r_ote
c      alpha = r_one / r_ialph
      fourpi = r_four * r_pi
      ee=sqrt( alpha * fourpi )
      sw=sqrt( sw2 )
      cw=sqrt( r_one - sw2 )
      ez=ee/(sw*cw)
      ey=ee*(sw/cw)
c
      gal(1) =  ee
      gal(2) =  ee
      gau(1) = -ee*r_two/r_three
      gau(2) = -ee*r_two/r_three
      gad(1) =  ee   /r_three
      gad(2) =  ee   /r_three
      gwf(1) = -ee/sqrt(r_two*sw2)
      gwf(2) =  r_zero
      gzn(1) = -ez*  r_half
      gzn(2) =  r_zero
      gzl(1) = -ez*(-r_half+sw2)
      gzl(2) = -ey
      gzu(1) = -ez*( r_half-sw2*r_two/r_three)
      gzu(2) =  ey*          r_two/r_three
      gzd(1) = -ez*(-r_half+sw2   /r_three)
      gzd(2) = -ey             /r_three
      g1(1)  =  r_one
      g1(2)  =  r_one
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine coup3x(sw2,zmass,hmass , 
     &                  gwwh,gzzh,ghhh,gwwhh,gzzhh,ghhhh)
c
c this subroutine sets up the coupling constants of the gauge bosons and
c higgs boson in the standard model.                                    
c                                                                       
c input:                                                                
c       real    sw2            : square of sine of the weak angle       
c       real    zmass          : mass of z                              
c       real    hmass          : mass of higgs                          
c                                                                       
c output:                                                               
c       real    gwwh           : dimensionful  coupling of w-,w+,h      
c       real    gzzh           : dimensionful  coupling of z, z, h      
c       real    ghhh           : dimensionful  coupling of h, h, h      
c       real    gwwhh          : dimensionful  coupling of w-,w+,h, h   
c       real    gzzhh          : dimensionful  coupling of z, z, h, h   
c       real    ghhhh          : dimensionless coupling of h, h, h, h   
c
      real*8    sw2,zmass,hmass,gwwh,gzzh,ghhh,gwwhh,gzzhh,ghhhh,
     &        alpha,fourpi,ee2,sc2,v
c
      real*8 r_half, r_one, r_two, r_three, r_four, r_ote
      real*8 r_pi, r_ialph
      parameter( r_half=0.5d0, r_one=1.0d0, r_two=2.0d0, r_three=3.0d0 )
      parameter( r_four=4.0d0, r_ote=128.0d0 )
      parameter( r_pi=3.14159265358979323846d0, r_ialph=137.0359895d0 )
c
      alpha = r_one / r_ote
c      alpha = r_one / r_ialph
      fourpi = r_four * r_pi
      ee2=alpha*fourpi
      sc2=sw2*( r_one - sw2 )
      v = r_two * zmass*sqrt(sc2)/sqrt(ee2)
c
      gwwh  =   ee2/sw2*r_half*v
      gzzh  =   ee2/sc2*r_half*v
      ghhh  =  -hmass**2/v*r_three
      gwwhh =   ee2/sw2*r_half
      gzzhh =   ee2/sc2*r_half
      ghhhh = -(hmass/v)**2*r_three
c
      return
      end subroutine
C
C ----------------------------------------------------------------------
C
      SUBROUTINE COUP4X(SW2,ZMASS,FMASS , GCHF)
C
C This subroutine sets up the coupling constant for the fermion-fermion-
C Higgs vertex in the STANDARD MODEL.  The coupling is COMPLEX and the  
C array of the coupling specifies the chirality of the flowing-IN       
C fermion.  GCHF(1) denotes a left-handed coupling, and GCHF(2) a right-
C handed coupling.                                                      
C                                                                       
C INPUT:                                                                
C       real    SW2            : square of sine of the weak angle       
C       real    ZMASS          : Z       mass                           
C       real    FMASS          : fermion mass                           
C                                                                       
C OUTPUT:                                                               
C       complex GCHF(2)        : coupling of fermion and Higgs          
C
      implicit none
      COMPLEX*16 GCHF(2)
      REAL*8    SW2,ZMASS,FMASS,ALPHA,FOURPI,EZ,G
C
      ALPHA=1.d0/128.d0
C      ALPHA=1./REAL(137.0359895)
      FOURPI=4.D0*3.14159265358979323846D0
      EZ=SQRT(ALPHA*FOURPI)/SQRT(SW2*(1.d0-SW2))
      G=EZ*FMASS*0.5d0/ZMASS
C
      GCHF(1) = DCMPLX( -G )
      GCHF(2) = DCMPLX( -G )
C
      RETURN
      end subroutine
C
C ======================================================================
C
      SUBROUTINE EAIXXX(EB,EA,SHLF,CHLF,PHI,NHE,NHA , EAI)
C
C This subroutine computes an off-shell electron wavefunction after     
C emitting a photon from the electron beam, with a special care for the 
C small angle region.  The momenta are measured in the laboratory frame,
C where the e- beam is along the positive z axis.                       
C                                                                       
C INPUT:                                                                
C       real    EB             : energy (GeV)    of beam  e-            
C       real    EA             : energy (GeV)    of final photon        
C       real    SHLF           : sin(theta/2)    of final photon        
C       real    CHLF           : cos(theta/2)    of final photon        
C       real    PHI            : azimuthal angle of final photon        
C       integer NHE  = -1 or 1 : helicity        of beam  e-            
C       integer NHA  = -1 or 1 : helicity        of final photon        
C                                                                       
C OUTPUT:                                                               
C       complex EAI(6)         : off-shell electron             |e',A,e>
C
      implicit none
      COMPLEX*16 EAI(6),PHS
      REAL*8  EB,EA,SHLF,CHLF,PHI,ME,ALPHA,GAL,RNHE,X,C,S,D,COEFF,
     &        XNNP,XNNM,SNP,CSP
      INTEGER NHE,NHA,NN
C
      ME   = 0.51099906D-3
      ALPHA=1./128.
      GAL  =SQRT(ALPHA*4.*3.14159265D0)
C
      NN=NHA*NHE
      RNHE=NHE
      X=EA/EB
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.*CHLF*SHLF
      D=-1./(EA*EB*(4.*SHLF**2+(ME/EB)**2*C))
      COEFF=-NN*GAL*SQRT(EB)*D
      XNNP=X*(1+NN)
      XNNM=X*(1-NN)
      SNP=SIN(PHI)
      CSP=COS(PHI)
      PHS=dCMPLX( CSP , RNHE*SNP )
C
      EAI((5-3*NHE)/2) = -RNHE*COEFF*ME*S*(1.+XNNP*.5)
      EAI((5-NHE)/2)   =  XNNP*COEFF*ME*CHLF**2*PHS
      EAI((5+NHE)/2)   =  RNHE*COEFF*EB*S*(-2.+XNNM)
      EAI((5+3*NHE)/2) =  XNNM*COEFF*EB*SHLF**2*PHS*2.
C
      EAI(5) =  EB*dCMPLX( 1.-X , 1.-X*C )
      EAI(6) = -EB*X*S*dCMPLX( CSP , SNP )
C
      RETURN
      end subroutine
C
C ----------------------------------------------------------------------
C
      SUBROUTINE EAOXXX(EB,EA,SHLF,CHLF,PHI,NHE,NHA , EAO)
C
C This subroutine computes an off-shell positron wavefunction after     
C emitting a photon from the positron beam, with a special care for the 
C small angle region.  The momenta are measured in the laboratory frame,
C where the e+ beam is along the negative z axis.                       
C                                                                       
C INPUT:                                                                
C       real    EB             : energy (GeV)    of beam  e+            
C       real    EA             : energy (GeV)    of final photon        
C       real    SHLF           : sin(theta/2)    of final photon        
C       real    CHLF           : cos(theta/2)    of final photon        
C       real    PHI            : azimuthal angle of final photon        
C       integer NHE  = -1 or 1 : helicity        of beam  e+            
C       integer NHA  = -1 or 1 : helicity        of final photon        
C                                                                       
C OUTPUT:                                                               
C       complex EAO(6)         : off-shell positron             <e,A,e'|
C
      implicit none
      COMPLEX*16 EAO(6),PHS
      REAL*8  EB,EA,SHLF,CHLF,PHI,ME,ALPHA,GAL,RNHE,X,C,S,D,COEFF,
     &        XNNP,XNNM,SNP,CSP
      INTEGER NHE,NHA,NN
C
      ME   = 0.51099906D-3
      ALPHA=1./128.
      GAL  =SQRT(ALPHA*4.*3.14159265D0)
C
      NN=NHA*NHE
      RNHE=NHE
      X=EA/EB
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.*CHLF*SHLF
      D=-1./(EA*EB*(4.*CHLF**2-(ME/EB)**2*C))
      COEFF=NN*GAL*SQRT(EB)*D
      XNNP=X*(1+NN)
      XNNM=X*(1-NN)
      SNP=SIN(PHI)
      CSP=COS(PHI)
      PHS=dCMPLX( CSP ,-RNHE*SNP )
C
      EAO((5-3*NHE)/2) =              COEFF*ME*S*(1.+XNNP*.5)
      EAO((5-NHE)/2)   = RNHE*XNNP    *COEFF*ME*SHLF**2*PHS
      EAO((5+NHE)/2)   =              COEFF*EB*S*(-2.+XNNM)
      EAO((5+3*NHE)/2) = REAL(NHA-NHE)*COEFF*EB*X*CHLF**2*PHS*2.
C
      EAO(5) = EB*dCMPLX( X-1. , X*C+1. )
      EAO(6) = EB*X*S*dCMPLX( CSP , SNP )
C
      RETURN
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine fsixxx(fi,sc,gc,fmass,fwidth , fsi)
c
c this subroutine computes an off-shell fermion wavefunction from a     
c flowing-in external fermion and a vector boson.                       
c                                                                       
c input:                                                                
c       complex*16 fi(6)          : flow-in  fermion                   |fi>
c       complex*16 sc(3)          : input    scalar                      s 
c       complex*16 gc(2)          : coupling constants                 gchf
c       real*8    fmass          : mass  of output fermion f'             
c       real*8    fwidth         : width of output fermion f'             
c                                                                       
c output:                                                               
c       complex fsi(6)         : off-shell fermion             |f',s,fi>
c
      complex*16 fi(6),sc(3),fsi(6),gc(2),sl1,sl2,sr1,sr2,ds
      real*8     pf(0:3),fmass,fwidth,pf2,p0p3,p0m3
c
      fsi(5) = fi(5)-sc(2)
      fsi(6) = fi(6)-sc(3)
c
      pf(0)=dble( fsi(5))
      pf(1)=dble( fsi(6))
      pf(2)=dimag(fsi(6))
      pf(3)=dimag(fsi(5))
      pf2=pf(0)**2-(pf(1)**2+pf(2)**2+pf(3)**2)
c
      ds=-sc(1)/dcmplx(pf2-fmass**2,max(dsign(fmass*fwidth ,pf2),0d0))
      p0p3=pf(0)+pf(3)
      p0m3=pf(0)-pf(3)
      sl1=gc(1)*(p0p3*fi(1)+dconjg(fsi(6))*fi(2))
      sl2=gc(1)*(p0m3*fi(2)      +fsi(6) *fi(1))
      sr1=gc(2)*(p0m3*fi(3)-dconjg(fsi(6))*fi(4))
      sr2=gc(2)*(p0p3*fi(4)      -fsi(6) *fi(3))
c
      fsi(1) = ( gc(1)*fmass*fi(1) + sr1 )*ds
      fsi(2) = ( gc(1)*fmass*fi(2) + sr2 )*ds
      fsi(3) = ( gc(2)*fmass*fi(3) + sl1 )*ds
      fsi(4) = ( gc(2)*fmass*fi(4) + sl2 )*ds
c
      return          
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine fsoxxx(fo,sc,gc,fmass,fwidth , fso)
c
c this subroutine computes an off-shell fermion wavefunction from a     
c flowing-out external fermion and a vector boson.                      
c                                                                       
c input:                                                                
c       complex*16 fo(6)          : flow-out fermion                   <fo|
c       complex*16 sc(6)          : input    scalar                      s 
c       complex*16 gc(2)          : coupling constants                 gchf
c       real*8     fmass          : mass  of output fermion f'             
c       real*8     fwidth         : width of output fermion f'             
c                                                                       
c output:                                                               
c       complex fso(6)         : off-shell fermion             <fo,s,f'|
c
      complex*16 fo(6),sc(6),fso(6),gc(2),sl1,sl2,sr1,sr2,ds
      real*8     pf(0:3),fmass,fwidth,pf2,p0p3,p0m3
c
      fso(5) = fo(5)+sc(2)
      fso(6) = fo(6)+sc(3)
c
      pf(0)=dble( fso(5))
      pf(1)=dble( fso(6))
      pf(2)=dimag(fso(6))
      pf(3)=dimag(fso(5))
      pf2=pf(0)**2-(pf(1)**2+pf(2)**2+pf(3)**2)
c
      ds=-sc(1)/dcmplx(pf2-fmass**2,max(dsign(fmass*fwidth ,pf2),0d0))
      p0p3=pf(0)+pf(3)
      p0m3=pf(0)-pf(3)
      sl1=gc(2)*(p0p3*fo(3)      +fso(6) *fo(4))
      sl2=gc(2)*(p0m3*fo(4)+dconjg(fso(6))*fo(3))
      sr1=gc(1)*(p0m3*fo(1)      -fso(6) *fo(2))
      sr2=gc(1)*(p0p3*fo(2)-dconjg(fso(6))*fo(1))
c
      fso(1) = ( gc(1)*fmass*fo(1) + sl1 )*ds
      fso(2) = ( gc(1)*fmass*fo(2) + sl2 )*ds
      fso(3) = ( gc(2)*fmass*fo(3) + sr1 )*ds
      fso(4) = ( gc(2)*fmass*fo(4) + sr2 )*ds
c
      return          
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine fvixxx(fi,vc,g,fmass,fwidth , fvi)
c
c this subroutine computes an off-shell fermion wavefunction from a     
c flowing-in external fermion and a vector boson.                       
c                                                                       
c input:                                                                
c       complex fi(6)          : flow-in  fermion                   |fi>
c       complex vc(6)          : input    vector                      v 
c       real    g(2)           : coupling constants                  gvf
c       real    fmass          : mass  of output fermion f'             
c       real    fwidth         : width of output fermion f'             
c                                                                       
c output:                                                               
c       complex fvi(6)         : off-shell fermion             |f',v,fi>
c
      complex*16 fi(6),vc(6),fvi(6),sl1,sl2,sr1,sr2,d
      real*8    g(2),pf(0:3),fmass,fwidth,pf2
c
      real*8 r_zero, r_one
      parameter( r_zero=0.0d0, r_one=1.0d0 )
      complex*16 c_imag
      c_imag=dcmplx( r_zero, r_one )
c
      fvi(5) = fi(5)-vc(5)
      fvi(6) = fi(6)-vc(6)
c
      pf(0)=dble( fvi(5))
      pf(1)=dble( fvi(6))
      pf(2)=dimag(fvi(6))
      pf(3)=dimag(fvi(5))
      pf2=pf(0)**2-(pf(1)**2+pf(2)**2+pf(3)**2)
c
      d=-r_one/dcmplx( pf2-fmass**2,max(sign(fmass*fwidth,pf2),r_zero))
      sl1= (vc(1)+       vc(4))*fi(1)
     &    +(vc(2)-c_imag*vc(3))*fi(2)
      sl2= (vc(2)+c_imag*vc(3))*fi(1)
     &    +(vc(1)-       vc(4))*fi(2)
c
      if ( g(2) .ne. r_zero ) then
         sr1= (vc(1)-       vc(4))*fi(3)
     &       -(vc(2)-c_imag*vc(3))*fi(4)
         sr2=-(vc(2)+c_imag*vc(3))*fi(3)
     &       +(vc(1)+       vc(4))*fi(4)
c
         fvi(1) = ( g(1)*((pf(0)-pf(3))*sl1 -dconjg(fvi(6))*sl2)
     &             +g(2)*fmass*sr1)*d
         fvi(2) = ( g(1)*(      -fvi(6)*sl1 +(pf(0)+pf(3))*sl2)
     &             +g(2)*fmass*sr2)*d
         fvi(3) = ( g(2)*((pf(0)+pf(3))*sr1 +dconjg(fvi(6))*sr2)
     &             +g(1)*fmass*sl1)*d
         fvi(4) = ( g(2)*(       fvi(6)*sr1 +(pf(0)-pf(3))*sr2)
     &             +g(1)*fmass*sl2)*d
c
      else          
         fvi(1) = g(1)*((pf(0)-pf(3))*sl1 -dconjg(fvi(6))*sl2)*d
         fvi(2) = g(1)*(      -fvi(6)*sl1 +(pf(0)+pf(3))*sl2)*d
         fvi(3) = g(1)*fmass*sl1*d
         fvi(4) = g(1)*fmass*sl2*d
      end if
c
      return          
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine fvoxxx(fo,vc,g,fmass,fwidth , fvo)
c
c this subroutine computes an off-shell fermion wavefunction from a     
c flowing-out external fermion and a vector boson.                      
c                                                                       
c input:                                                                
c       complex fo(6)          : flow-out fermion                   <fo|
c       complex vc(6)          : input    vector                      v 
c       real    g(2)           : coupling constants                  gvf
c       real    fmass          : mass  of output fermion f'             
c       real    fwidth         : width of output fermion f'             
c                                                                       
c output:                                                               
c       complex fvo(6)         : off-shell fermion             <fo,v,f'|
c
      complex*16 fo(6),vc(6),fvo(6),sl1,sl2,sr1,sr2,d
      real*8    g(2),pf(0:3),fmass,fwidth,pf2
c
      real*8 r_zero, r_one
      parameter( r_zero=0.0d0, r_one=1.0d0 )
      complex*16 c_imag
      c_imag=dcmplx( r_zero, r_one )
c
      fvo(5) = fo(5)+vc(5)
      fvo(6) = fo(6)+vc(6)
c
      pf(0)=dble( fvo(5))
      pf(1)=dble( fvo(6))
      pf(2)=dimag(fvo(6))
      pf(3)=dimag(fvo(5))
      pf2=pf(0)**2-(pf(1)**2+pf(2)**2+pf(3)**2)
c
      d=-r_one/dcmplx( pf2-fmass**2,max(sign(fmass*fwidth,pf2),r_zero))
      sl1= (vc(1)+       vc(4))*fo(3)
     &    +(vc(2)+c_imag*vc(3))*fo(4)
      sl2= (vc(2)-c_imag*vc(3))*fo(3)
     &    +(vc(1)-       vc(4))*fo(4)
c
      if ( g(2) .ne. r_zero ) then
         sr1= (vc(1)-       vc(4))*fo(1)
     &       -(vc(2)+c_imag*vc(3))*fo(2)
         sr2=-(vc(2)-c_imag*vc(3))*fo(1)
     &       +(vc(1)+       vc(4))*fo(2)
c
         fvo(1) = ( g(2)*( (pf(0)+pf(3))*sr1        +fvo(6)*sr2)
     &             +g(1)*fmass*sl1)*d
         fvo(2) = ( g(2)*( dconjg(fvo(6))*sr1 +(pf(0)-pf(3))*sr2)
     &             +g(1)*fmass*sl2)*d
         fvo(3) = ( g(1)*( (pf(0)-pf(3))*sl1        -fvo(6)*sl2)
     &             +g(2)*fmass*sr1)*d
         fvo(4) = ( g(1)*(-dconjg(fvo(6))*sl1 +(pf(0)+pf(3))*sl2)
     &             +g(2)*fmass*sr2)*d
c
      else
         fvo(1) = g(1)*fmass*sl1*d
         fvo(2) = g(1)*fmass*sl2*d
         fvo(3) = g(1)*( (pf(0)-pf(3))*sl1        -fvo(6)*sl2)*d
         fvo(4) = g(1)*(-dconjg(fvo(6))*sl1 +(pf(0)+pf(3))*sl2)*d
      end if
c
      return          
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine ggggxx(wm,w31,wp,w32,g, vertex)
c
c this subroutine computes an amplitude of the four-point coupling of   
c the w-, w+ and two w3/z/a.  the amplitude includes the contributions  
c of w exchange diagrams.  the internal w propagator is given in unitary
c gauge.  if one sets wmass=0.0, then the gggg vertex is given (see sect
c 2.9.1 of the manual).
c                                                                       
c input:                                                                
c       complex wm(0:3)        : flow-out w-                         wm 
c       complex w31(0:3)       : first    w3/z/a                     w31
c       complex wp(0:3)        : flow-out w+                         wp 
c       complex w32(0:3)       : second   w3/z/a                     w32
c       real    g              : coupling of w31 with w-/w+             
c                                                  (see the table below)
c                                                                       
c the possible sets of the inputs are as follows:                       
c   -------------------------------------------                         
c   |  wm  |  w31 |  wp  |  w32 |  g31 |  g32 |                         
c   -------------------------------------------                         
c   |  w-  |  w3  |  w+  |  w3  |  gw  |  gw  |                         
c   |  w-  |  w3  |  w+  |  z   |  gw  | gwwz |                         
c   |  w-  |  w3  |  w+  |  a   |  gw  | gwwa |                         
c   |  w-  |  z   |  w+  |  z   | gwwz | gwwz |                         
c   |  w-  |  z   |  w+  |  a   | gwwz | gwwa |                         
c   |  w-  |  a   |  w+  |  a   | gwwa | gwwa |                         
c   -------------------------------------------                         
c where all the bosons are defined by the flowing-out quantum number.   
c                                                                       
c output:                                                               
c       complex vertex         : amplitude          gamma(wm,w31,wp,w32)
c
      implicit none
      complex*16    wm(6),w31(6),wp(6),w32(6),vertex
      complex*16 dv1(0:3),dv2(0:3),dv3(0:3),dv4(0:3),
     &           dvertx,v12,v13,v14,v23,v24,v34
      real*8       pwm(0:3),pw31(0:3),pwp(0:3),pw32(0:3),g
      real*8     dp1(0:3),dp2(0:3),dp3(0:3),dp4(0:3)
c
      real*8 r_zero, r_one
      parameter( r_zero=0.0d0, r_one=1.0d0 )
c
      pwm(0)=dble( wm(5))
      pwm(1)=dble( wm(6))
      pwm(2)=dimag(wm(6))
      pwm(3)=dimag(wm(5))
      pwp(0)=dble( wp(5))
      pwp(1)=dble( wp(6))
      pwp(2)=dimag(wp(6))
      pwp(3)=dimag(wp(5))
      pw31(0)=dble( w31(5))
      pw31(1)=dble( w31(6))
      pw31(2)=dimag(w31(6))
      pw31(3)=dimag(w31(5))
      pw32(0)=dble( w32(5))
      pw32(1)=dble( w32(6))
      pw32(2)=dimag(w32(6))
      pw32(3)=dimag(w32(5))
c
      dv1(0)=dcmplx(wm(1))
      dv1(1)=dcmplx(wm(2))
      dv1(2)=dcmplx(wm(3))
      dv1(3)=dcmplx(wm(4))
      dp1(0)=dble(pwm(0))
      dp1(1)=dble(pwm(1))
      dp1(2)=dble(pwm(2))
      dp1(3)=dble(pwm(3))
      dv2(0)=dcmplx(w31(1))
      dv2(1)=dcmplx(w31(2))
      dv2(2)=dcmplx(w31(3))
      dv2(3)=dcmplx(w31(4))
      dp2(0)=dble(pw31(0))
      dp2(1)=dble(pw31(1))
      dp2(2)=dble(pw31(2))
      dp2(3)=dble(pw31(3))
      dv3(0)=dcmplx(wp(1))
      dv3(1)=dcmplx(wp(2))
      dv3(2)=dcmplx(wp(3))
      dv3(3)=dcmplx(wp(4))
      dp3(0)=dble(pwp(0))
      dp3(1)=dble(pwp(1))
      dp3(2)=dble(pwp(2))
      dp3(3)=dble(pwp(3))
      dv4(0)=dcmplx(w32(1))
      dv4(1)=dcmplx(w32(2))
      dv4(2)=dcmplx(w32(3))
      dv4(3)=dcmplx(w32(4))
      dp4(0)=dble(pw32(0))
      dp4(1)=dble(pw32(1))
      dp4(2)=dble(pw32(2))
      dp4(3)=dble(pw32(3))
c
      v12= dv1(0)*dv2(0)-dv1(1)*dv2(1)-dv1(2)*dv2(2)-dv1(3)*dv2(3)
      v13= dv1(0)*dv3(0)-dv1(1)*dv3(1)-dv1(2)*dv3(2)-dv1(3)*dv3(3)
      v14= dv1(0)*dv4(0)-dv1(1)*dv4(1)-dv1(2)*dv4(2)-dv1(3)*dv4(3)
      v23= dv2(0)*dv3(0)-dv2(1)*dv3(1)-dv2(2)*dv3(2)-dv2(3)*dv3(3)
      v24= dv2(0)*dv4(0)-dv2(1)*dv4(1)-dv2(2)*dv4(2)-dv2(3)*dv4(3)
      v34= dv3(0)*dv4(0)-dv3(1)*dv4(1)-dv3(2)*dv4(2)-dv3(3)*dv4(3)

         dvertx = v14*v23 -v13*v24
c
         vertex = dcmplx( dvertx ) * (g*g)
c
      return
      end subroutine
c
c ======================================================================
c
      subroutine gggxxx(wm,wp,w3,g , vertex)
c
c this subroutine computes an amplitude of the three-point coupling of  
c the gauge bosons.                                                     
c                                                                       
c input:                                                                
c       complex wm(6)          : vector               flow-out w-       
c       complex wp(6)          : vector               flow-out w+       
c       complex w3(6)          : vector               j3 or a    or z   
c       real    g              : coupling constant    gw or gwwa or gwwz
c                                                                       
c output:                                                               
c       complex vertex         : amplitude               gamma(wm,wp,w3)
c
      complex*16 wm(6),wp(6),w3(6),vertex, 
     &        xv1,xv2,xv3,v12,v23,v31,p12,p13,p21,p23,p31,p32
      real*8    pwm(0:3),pwp(0:3),pw3(0:3),g
c
      real*8 r_zero, r_tenth
      parameter( r_zero=0.0d0, r_tenth=0.1d0 )
c
      pwm(0)=dble( wm(5))
      pwm(1)=dble( wm(6))
      pwm(2)=dimag(wm(6))
      pwm(3)=dimag(wm(5))
      pwp(0)=dble( wp(5))
      pwp(1)=dble( wp(6))
      pwp(2)=dimag(wp(6))
      pwp(3)=dimag(wp(5))
      pw3(0)=dble( w3(5))
      pw3(1)=dble( w3(6))
      pw3(2)=dimag(w3(6))
      pw3(3)=dimag(w3(5))
c
      v12=wm(1)*wp(1)-wm(2)*wp(2)-wm(3)*wp(3)-wm(4)*wp(4)
      v23=wp(1)*w3(1)-wp(2)*w3(2)-wp(3)*w3(3)-wp(4)*w3(4)
      v31=w3(1)*wm(1)-w3(2)*wm(2)-w3(3)*wm(3)-w3(4)*wm(4)
      xv1=r_zero
      xv2=r_zero
      xv3=r_zero
      if ( abs(wm(1)) .ne. r_zero ) then
         if (abs(wm(1)).ge.max(abs(wm(2)),abs(wm(3)),abs(wm(4)))
     $        *r_tenth)
     &      xv1=pwm(0)/wm(1)
      endif
      if ( abs(wp(1)) .ne. r_zero) then
         if (abs(wp(1)).ge.max(abs(wp(2)),abs(wp(3)),abs(wp(4)))
     $        *r_tenth)
     &      xv2=pwp(0)/wp(1)
      endif
      if ( abs(w3(1)) .ne. r_zero) then
         if ( abs(w3(1)).ge.max(abs(w3(2)),abs(w3(3)),abs(w3(4)))
     $        *r_tenth)
     &      xv3=pw3(0)/w3(1)
      endif
      p12= (pwm(0)-xv1*wm(1))*wp(1)-(pwm(1)-xv1*wm(2))*wp(2)
     &    -(pwm(2)-xv1*wm(3))*wp(3)-(pwm(3)-xv1*wm(4))*wp(4)
      p13= (pwm(0)-xv1*wm(1))*w3(1)-(pwm(1)-xv1*wm(2))*w3(2)
     &    -(pwm(2)-xv1*wm(3))*w3(3)-(pwm(3)-xv1*wm(4))*w3(4)
      p21= (pwp(0)-xv2*wp(1))*wm(1)-(pwp(1)-xv2*wp(2))*wm(2)
     &    -(pwp(2)-xv2*wp(3))*wm(3)-(pwp(3)-xv2*wp(4))*wm(4)
      p23= (pwp(0)-xv2*wp(1))*w3(1)-(pwp(1)-xv2*wp(2))*w3(2)
     &    -(pwp(2)-xv2*wp(3))*w3(3)-(pwp(3)-xv2*wp(4))*w3(4)
      p31= (pw3(0)-xv3*w3(1))*wm(1)-(pw3(1)-xv3*w3(2))*wm(2)
     &    -(pw3(2)-xv3*w3(3))*wm(3)-(pw3(3)-xv3*w3(4))*wm(4)
      p32= (pw3(0)-xv3*w3(1))*wp(1)-(pw3(1)-xv3*w3(2))*wp(2)
     &    -(pw3(2)-xv3*w3(3))*wp(3)-(pw3(3)-xv3*w3(4))*wp(4)
c
      vertex = -(v12*(p13-p23)+v23*(p21-p31)+v31*(p32-p12))*g
c
      return
      end subroutine
      subroutine hioxxx(fi,fo,gc,smass,swidth , hio)                         
c                                                                            
c this subroutine computes an off-shell scalar current from an external       
c fermion pair.                                                               
c                                                                              
c input:                                                                      
c       complex fi(6)          : flow-in  fermion                   |fi>     
c       complex fo(6)          : flow-out fermion                   <fo|       
c       complex gc(2)          : coupling constants                 gchf       
c       real    smass          : mass  of output scalar s                      
c       real    swidth         : width of output scalar s                      
c                                                                              
c output:                                                                      
c       complex hio(3)         : scalar current             j(<fi|s|fo>)       
c                                                                              
      complex*16 fi(6),fo(6),hio(3),gc(2),dn                                   
      real*8  q(0:3),smass,swidth,q2                                           
c                                                                              
      hio(2) = fo(5)-fi(5)                                                     
      hio(3) = fo(6)-fi(6)                                                     
c                                                                              
      q(0)=dble( hio(2))                                                       
      q(1)=dble( hio(3))                                                       
      q(2)=dimag(hio(3))                                                       
      q(3)=dimag(hio(2))                                                       
      q2=q(0)**2-(q(1)**2+q(2)**2+q(3)**2)                                     
c                                                                              
      dn=-dcmplx(q2-smass**2,dmax1(dsign(smass*swidth,q2),0.d0))               
c                                                                              
      hio(1) = ( gc(1)*(fo(1)*fi(1)+fo(2)*fi(2))                               
     &          +gc(2)*(fo(3)*fi(3)+fo(4)*fi(4)) )/dn                          
c                                                                              
      return                                                                   
      end subroutine

C ----------------------------------------------------------------------
C
      SUBROUTINE HSSSXX(S1,S2,S3,G,SMASS,SWIDTH , HSSS)
C
C This subroutine computes an off-shell scalar current from the four-   
C scalar coupling.                                                      
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex S3(3)          : third  scalar                        S3
C       real    G              : coupling constant                 GHHHH
C       real    SMASS          : mass  of OUTPUT scalar S'              
C       real    SWIDTH         : width of OUTPUT scalar S'              
C                                                                       
C OUTPUT:                                                               
C       complex HSSS(3)        : scalar current           J(S':S1,S2,S3)
C
      implicit none
      COMPLEX*16 S1(3),S2(3),S3(3),HSSS(3),DG
      REAL*8     Q(0:3),G,SMASS,SWIDTH,Q2
C
      HSSS(2) = S1(2)+S2(2)+S3(2)
      HSSS(3) = S1(3)+S2(3)+S3(3)
C
      Q(0)=dble( HSSS(2))
      Q(1)=dble( HSSS(3))
      Q(2)=dIMAG(HSSS(3))
      Q(3)=dIMAG(HSSS(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DG=-G/dCMPLX( Q2-SMASS**2,MAX(SIGN(SMASS*SWIDTH ,Q2),0.d0))
C
      HSSS(1) = DG * S1(1)*S2(1)*S3(1)
C
      RETURN
      end subroutine
C ----------------------------------------------------------------------
C
      SUBROUTINE HSSXXX(S1,S2,G,SMASS,SWIDTH , HSS)
C
C This subroutine computes an off-shell scalar current from the three-  
C scalar coupling.                                                      
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant                  GHHH
C       real    SMASS          : mass  of OUTPUT scalar S'              
C       real    SWIDTH         : width of OUTPUT scalar S'              
C                                                                       
C OUTPUT:                                                               
C       complex HSS(3)         : scalar current              J(S':S1,S2)
C
      implicit none
      COMPLEX*16 S1(3),S2(3),HSS(3),DG
      REAL*8  Q(0:3),G,SMASS,SWIDTH,Q2
C
      HSS(2) = S1(2)+S2(2)
      HSS(3) = S1(3)+S2(3)
C
      Q(0)=dble( HSS(2))
      Q(1)=dble( HSS(3))
      Q(2)=dIMAG(HSS(3))
      Q(3)=dIMAG(HSS(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DG=-G/dCMPLX( Q2-SMASS**2, MAX(SIGN(SMASS*SWIDTH ,Q2),0.d0))
C
      HSS(1) = DG*S1(1)*S2(1)
C
      RETURN
      end subroutine
C
C ======================================================================
c ----------------------------------------------------------------------
c
      subroutine hvsxxx(vc,sc,g,smass,swidth , hvs)
c
c this subroutine computes an off-shell scalar current from the vector- 
c scalar-scalar coupling.  the coupling is absent in the minimal sm in  
c unitary gauge.                                                        
c                                                                       
c input:                                                                
c       complex vc(6)          : input vector                          v
c       complex sc(3)          : input scalar                          s
c       complex g              : coupling constant (s charge)           
c       real    smass          : mass  of output scalar s'              
c       real    swidth         : width of output scalar s'              
c                                                                       
c examples of the coupling constant g for susy particles are as follows:
c   -----------------------------------------------------------         
c   |    s1    | (q,i3) of s1  ||   v=a   |   v=z   |   v=w   |         
c   -----------------------------------------------------------         
c   | nu~_l    | (  0  , +1/2) ||   ---   |  gzn(1) |  gwf(1) |         
c   | e~_l     | ( -1  , -1/2) ||  gal(1) |  gzl(1) |  gwf(1) |         
c   | u~_l     | (+2/3 , +1/2) ||  gau(1) |  gzu(1) |  gwf(1) |         
c   | d~_l     | (-1/3 , -1/2) ||  gad(1) |  gzd(1) |  gwf(1) |         
c   -----------------------------------------------------------         
c   | e~_r-bar | ( +1  ,  0  ) || -gal(2) | -gzl(2) | -gwf(2) |         
c   | u~_r-bar | (-2/3 ,  0  ) || -gau(2) | -gzu(2) | -gwf(2) |         
c   | d~_r-bar | (+1/3 ,  0  ) || -gad(2) | -gzd(2) | -gwf(2) |         
c   -----------------------------------------------------------         
c where the sc charge is defined by the flowing-out quantum number.     
c                                                                       
c output:                                                               
c       complex hvs(3)         : scalar current                j(s':v,s)
c
      implicit none
      complex*16 vc(6),sc(3),hvs(3),dg,qvv,qpv,g
      real*8    qv(0:3),qp(0:3),qa(0:3),smass,swidth,q2
c
      hvs(2) = vc(5)+sc(2)
      hvs(3) = vc(6)+sc(3)
c
      qv(0)=dble(  vc(5))
      qv(1)=dble(  vc(6))
      qv(2)=dimag( vc(6))
      qv(3)=dimag( vc(5))
      qp(0)=dble(  sc(2))
      qp(1)=dble(  sc(3))
      qp(2)=dimag( sc(3))
      qp(3)=dimag( sc(2))
      qa(0)=dble( hvs(2))
      qa(1)=dble( hvs(3))
      qa(2)=dimag(hvs(3))
      qa(3)=dimag(hvs(2))
      q2=qa(0)**2-(qa(1)**2+qa(2)**2+qa(3)**2)
c
      dg=-g/dcmplx( q2-smass**2 , max(dsign( smass*swidth ,q2),0d0) )
      qvv=qv(0)*vc(1)-qv(1)*vc(2)-qv(2)*vc(3)-qv(3)*vc(4)
      qpv=qp(0)*vc(1)-qp(1)*vc(2)-qp(2)*vc(3)-qp(3)*vc(4)
c
      hvs(1) = dg*(2d0*qpv+qvv)*sc(1)
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine hvvxxx(v1,v2,g,smass,swidth , hvv)
c
c this subroutine computes an off-shell scalar current from the vector- 
c vector-scalar coupling.                                               
c                                                                       
c input:                                                                
c       complex v1(6)          : first  vector                        v1
c       complex v2(6)          : second vector                        v2
c       real    g              : coupling constant                  gvvh
c       real    smass          : mass  of output scalar s               
c       real    swidth         : width of output scalar s               
c                                                                       
c output:                                                               
c       complex hvv(3)         : off-shell scalar current     j(s:v1,v2)
c
      complex*16 v1(6),v2(6),hvv(3),dg
      real*8    q(0:3),g,smass,swidth,q2
c
      real*8 r_zero
      parameter( r_zero=0.0d0 )
c
      hvv(2) = v1(5)+v2(5)
      hvv(3) = v1(6)+v2(6)
c
      q(0)=dble( hvv(2))
      q(1)=dble( hvv(3))
      q(2)=dimag(hvv(3))
      q(3)=dimag(hvv(2))
      q2=q(0)**2-(q(1)**2+q(2)**2+q(3)**2)
c
      dg=-g/dcmplx( q2-smass**2 , max(sign( smass*swidth ,q2),r_zero) )
c
      hvv(1) = dg*(v1(1)*v2(1)-v1(2)*v2(2)-v1(3)*v2(3)-v1(4)*v2(4))
c
      return
      end subroutine
C
C ======================================================================
C
      SUBROUTINE IOSXXX(FI,FO,SC,GC , VERTEX)
C
C This subroutine computes an amplitude of the fermion-fermion-scalar   
C coupling.                                                             
C                                                                       
C INPUT:                                                                
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex SC(3)          : input    scalar                      S 
C       complex GC(2)          : coupling constants                 GCHF
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude                     <FO|S|FI>
C
      COMPLEX*16 FI(6),FO(6),SC(3),GC(2),VERTEX
C
      VERTEX = SC(1)*( GC(1)*(FI(1)*FO(1)+FI(2)*FO(2))
     &                +GC(2)*(FI(3)*FO(3)+FI(4)*FO(4)) )
C
      RETURN          
      end subroutine
c
c ======================================================================
c
      subroutine iovxxx(fi,fo,vc,g , vertex)
c
c this subroutine computes an amplitude of the fermion-fermion-vector   
c coupling.                                                             
c                                                                       
c input:                                                                
c       complex fi(6)          : flow-in  fermion                   |fi>
c       complex fo(6)          : flow-out fermion                   <fo|
c       complex vc(6)          : input    vector                      v 
c       real    g(2)           : coupling constants                  gvf
c                                                                       
c output:                                                               
c       complex vertex         : amplitude                     <fo|v|fi>
c
      complex*16 fi(6),fo(6),vc(6),vertex
      real*8    g(2)
c
      real*8 r_zero, r_one
      parameter( r_zero=0.0d0, r_one=1.0d0 )
      complex*16 c_imag
      c_imag=dcmplx( r_zero, r_one )
c

      vertex =  g(1)*( (fo(3)*fi(1)+fo(4)*fi(2))*vc(1)
     &                +(fo(3)*fi(2)+fo(4)*fi(1))*vc(2)
     &                -(fo(3)*fi(2)-fo(4)*fi(1))*vc(3)*c_imag
     &                +(fo(3)*fi(1)-fo(4)*fi(2))*vc(4)        )
c
      if ( g(2) .ne. r_zero ) then
         vertex = vertex
     &          + g(2)*( (fo(1)*fi(3)+fo(2)*fi(4))*vc(1)
     &                  -(fo(1)*fi(4)+fo(2)*fi(3))*vc(2)
     &                  +(fo(1)*fi(4)-fo(2)*fi(3))*vc(3)*c_imag
     &                  -(fo(1)*fi(3)-fo(2)*fi(4))*vc(4)        )
      end if
c
      return
      end subroutine
c
c	Subroutine returns the desired fermion or
c	anti-fermion spinor. ie., |f>
c	A replacement for the HELAS routine IXXXXX
c
c	Adam Duff,  1992 August 31
c	<duff@phenom.physics.wisc.edu>
c
      subroutine ixxxxx(
     &              p,		!in: four vector momentum
     &              fmass,	!in: fermion mass
     &              nhel,	!in: spinor helicity, -1 or 1
     &              nsf,	!in: -1=antifermion, 1=fermion
     &              fi		!out: fermion wavefunction
     &                 )
      implicit none
c
c declare input/output variables
c
      complex*16 fi(6)
      integer*4 nhel, nsf
      real*8 p(0:3), fmass
c
c declare local variables
c
      real*8 r_zero, r_one, r_two
      parameter( r_zero=0.0d0, r_one=1.0d0, r_two=2.0d0 )
      complex*16 c_zero
c
      real*8 plat, pabs, omegap, omegam, rs2pa, spaz
      c_zero=dcmplx( r_zero, r_zero )
c
c define kinematic parameters
c
      fi(5) = dcmplx( p(0), p(3) ) * nsf
      fi(6) = dcmplx( p(1), p(2) ) * nsf
      plat = sqrt( p(1)**2 + p(2)**2 )
      pabs = sqrt( p(1)**2 + p(2)**2 + p(3)**2 )
      omegap = sqrt( p(0) + pabs )
c
c do massive fermion case
c
      if ( fmass .ne. r_zero ) then
         omegam = fmass / omegap
         if ( nsf .eq. 1 ) then
            if ( nhel .eq. 1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fi(1) = dcmplx( omegam, r_zero )
                     fi(2) = c_zero
                     fi(3) = dcmplx( omegap, r_zero )
                     fi(4) = c_zero
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs + p(3) )
                     fi(1) = omegam * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fi(2) = omegam * rs2pa / spaz
     &                     * dcmplx( p(1), p(2) )
                     fi(3) = omegap * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fi(4) = omegap * rs2pa / spaz
     &                     * dcmplx( p(1), p(2) )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = dcmplx( omegam, r_zero )
                     fi(3) = c_zero
                     fi(4) = dcmplx( omegap, r_zero )
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs - p(3) )
                     fi(1) = omegam * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(2) = omegam * rs2pa * spaz / plat
     &                     * dcmplx( p(1), p(2) )
                     fi(3) = omegap * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(4) = omegap * rs2pa * spaz / plat
     &                     * dcmplx( p(1), p(2) )
                  end if
               end if
            else if ( nhel .eq. -1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = dcmplx( omegap, r_zero )
                     fi(3) = c_zero
                     fi(4) = dcmplx( omegam, r_zero )
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs + p(3) )
                     fi(1) = omegap * rs2pa / spaz
     &                     * dcmplx( -p(1), p(2) )
                     fi(2) = omegap * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fi(3) = omegam * rs2pa / spaz
     &                     * dcmplx( -p(1), p(2) )
                     fi(4) = omegam * rs2pa
     &                     * dcmplx( spaz, r_zero )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fi(1) = dcmplx( -omegap, r_zero )
                     fi(2) = c_zero
                     fi(3) = dcmplx( -omegam, r_zero )
                     fi(4) = c_zero
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs - p(3) )
                     fi(1) = omegap * rs2pa * spaz / plat
     &                     * dcmplx( -p(1), p(2) )
                     fi(2) = omegap * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(3) = omegam * rs2pa * spaz / plat
     &                     * dcmplx( -p(1), p(2) )
                     fi(4) = omegam * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                  end if
               end if
            else
               stop 'ixxxxx:  fermion helicity must be +1,-1'
            end if
         else if ( nsf .eq. -1 ) then
            if ( nhel .eq. 1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = dcmplx( -omegap, r_zero )
                     fi(3) = c_zero
                     fi(4) = dcmplx( omegam, r_zero )
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs + p(3) )
                     fi(1) = -omegap * rs2pa / spaz
     &                     * dcmplx( -p(1), p(2) )
                     fi(2) = -omegap * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fi(3) = omegam * rs2pa / spaz
     &                     * dcmplx( -p(1), p(2) )
                     fi(4) = omegam * rs2pa
     &                     * dcmplx( spaz, r_zero )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fi(1) = dcmplx( omegap, r_zero )
                     fi(2) = c_zero
                     fi(3) = dcmplx( -omegam, r_zero )
                     fi(4) = c_zero
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs - p(3) )
                     fi(1) = -omegap * rs2pa * spaz / plat
     &                     * dcmplx( -p(1), p(2) )
                     fi(2) = -omegap * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(3) = omegam * rs2pa * spaz / plat
     &                     * dcmplx( -p(1), p(2) )
                     fi(4) = omegam * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                  end if
               end if
            else if ( nhel .eq. -1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fi(1) = dcmplx( omegam, r_zero )
                     fi(2) = c_zero
                     fi(3) = dcmplx( -omegap, r_zero )
                     fi(4) = c_zero
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs + p(3) )
                     fi(1) = omegam * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fi(2) = omegam * rs2pa / spaz
     &                     * dcmplx( p(1), p(2) )
                     fi(3) = -omegap * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fi(4) = -omegap * rs2pa / spaz
     &                     * dcmplx( p(1), p(2) )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = dcmplx( omegam, r_zero )
                     fi(3) = c_zero
                     fi(4) = dcmplx( -omegap, r_zero )
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs - p(3) )
                     fi(1) = omegam * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(2) = omegam * rs2pa * spaz / plat
     &                     * dcmplx( p(1), p(2) )
                     fi(3) = -omegap * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(4) = -omegap * rs2pa * spaz / plat
     &                     * dcmplx( p(1), p(2) )
                  end if
               end if
            else
               stop 'ixxxxx:  fermion helicity must be +1,-1'
            end if
         else
            stop 'ixxxxx:  fermion type must be +1,-1'
         end if
c
c do massless fermion case
c
      else
         if ( nsf .eq. 1 ) then
            if ( nhel .eq. 1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = c_zero
                     fi(3) = dcmplx( omegap, r_zero )
                     fi(4) = c_zero
                  else
                     spaz = sqrt( pabs + p(3) )
                     fi(1) = c_zero
                     fi(2) = c_zero
                     fi(3) = dcmplx( spaz, r_zero )
                     fi(4) = r_one / spaz
     &                     * dcmplx( p(1), p(2) )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = c_zero
                     fi(3) = c_zero
                     fi(4) = dcmplx( omegap, r_zero )
                  else
                     spaz = sqrt( pabs - p(3) )
                     fi(1) = c_zero
                     fi(2) = c_zero
                     fi(3) = r_one / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(4) = spaz / plat
     &                     * dcmplx( p(1), p(2) )
                  end if
               end if
            else if ( nhel .eq. -1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = dcmplx( omegap, r_zero )
                     fi(3) = c_zero
                     fi(4) = c_zero
                  else
                     spaz = sqrt( pabs + p(3) )
                     fi(1) = r_one / spaz
     &                     * dcmplx( -p(1), p(2) )
                     fi(2) = dcmplx( spaz, r_zero )
                     fi(3) = c_zero
                     fi(4) = c_zero
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fi(1) = dcmplx( -omegap, r_zero )
                     fi(2) = c_zero
                     fi(3) = c_zero
                     fi(4) = c_zero
                  else
                     spaz = sqrt( pabs - p(3) )
                     fi(1) = spaz / plat
     &                     * dcmplx( -p(1), p(2) )
                     fi(2) = r_one / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(3) = c_zero
                     fi(4) = c_zero
                  end if
               end if
            else
               stop 'ixxxxx:  fermion helicity must be +1,-1'
            end if
         else if ( nsf .eq. -1 ) then
            if ( nhel .eq. 1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = dcmplx( -omegap, r_zero )
                     fi(3) = c_zero
                     fi(4) = c_zero
                  else
                     spaz = sqrt( pabs + p(3) )
                     fi(1) = -r_one / spaz
     &                     * dcmplx( -p(1), p(2) )
                     fi(2) = dcmplx( -spaz, r_zero )
                     fi(3) = c_zero
                     fi(4) = c_zero
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fi(1) = dcmplx( omegap, r_zero )
                     fi(2) = c_zero
                     fi(3) = c_zero
                     fi(4) = c_zero
                  else
                     spaz = sqrt( pabs - p(3) )
                     fi(1) = -spaz / plat
     &                     * dcmplx( -p(1), p(2) )
                     fi(2) = -r_one / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(3) = c_zero
                     fi(4) = c_zero
                  end if
               end if
            else if ( nhel .eq. -1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = c_zero
                     fi(3) = dcmplx( -omegap, r_zero )
                     fi(4) = c_zero
                  else
                     spaz = sqrt( pabs + p(3) )
                     fi(1) = c_zero
                     fi(2) = c_zero
                     fi(3) = dcmplx( -spaz, r_zero )
                     fi(4) = -r_one / spaz
     &                     * dcmplx( p(1), p(2) )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fi(1) = c_zero
                     fi(2) = c_zero
                     fi(3) = c_zero
                     fi(4) = dcmplx( -omegap, r_zero )
                  else
                     spaz = sqrt( pabs - p(3) )
                     fi(1) = c_zero
                     fi(2) = c_zero
                     fi(3) = -r_one / spaz
     &                     * dcmplx( plat, r_zero )
                     fi(4) = -spaz / plat
     &                     * dcmplx( p(1), p(2) )
                  end if
               end if
            else
               stop 'ixxxxx:  fermion helicity must be +1,-1'
            end if
         else
            stop 'ixxxxx:  fermion type must be +1,-1'
         end if
      end if
c
c done
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine j3xxxx(fi,fo,gaf,gzf,zmass,zwidth , j3)
c
c this subroutine computes the sum of photon and z currents with the    
c suitable weights ( j(w3) = cos(theta_w) j(z) + sin(theta_w) j(a) ).   
c the output j3 is useful as an input of vvvxxx, jvvxxx or w3w3xx.      
c the photon propagator is given in feynman gauge, and the z propagator 
c is given in unitary gauge.                                            
c                                                                       
c input:                                                                
c       complex fi(6)          : flow-in  fermion                   |fi>
c       complex fo(6)          : flow-out fermion                   <fo|
c       real    gaf(2)         : fi couplings with a                 gaf
c       real    gzf(2)         : fi couplings with z                 gzf
c       real    zmass          : mass  of z                             
c       real    zwidth         : width of z                             
c                                                                       
c output:                                                               
c       complex j3(6)          : w3 current             j^mu(<fo|w3|fi>)
c
      complex*16 fi(6),fo(6),j3(6),
     &        c0l,c1l,c2l,c3l,csl,c0r,c1r,c2r,c3r,csr,dz,ddif
      real*8    gaf(2),gzf(2),q(0:3),zmass,zwidth,zm2,zmw,q2,da,ww,
     &        cw,sw,gn,gz3l,ga3l
c
      real*8 r_zero, r_one
      parameter( r_zero=0.0d0, r_one=1.0d0 )
      complex*16 c_imag
      c_imag=dcmplx( r_zero, r_one )
c
      j3(5) = fo(5)-fi(5)
      j3(6) = fo(6)-fi(6)
c
      q(0)=-dble( j3(5))
      q(1)=-dble( j3(6))
      q(2)=-dimag(j3(6))
      q(3)=-dimag(j3(5))
      q2=q(0)**2-(q(1)**2+q(2)**2+q(3)**2)
      zm2=zmass**2
      zmw=zmass*zwidth
c
      da=r_one/q2
      ww=max(dsign( zmw ,q2),r_zero)
      dz=r_one/dcmplx( q2-zm2 , ww )
      ddif=dcmplx( -zm2 , ww )*da*dz
c
c ddif is the difference : ddif=da-dz
c  for the running width, use below instead of the above ww,dz and ddif.
c      ww=max( zwidth*q2/zmass ,r_zero)
c      dz=r_one/dcmplx( q2-zm2 , ww )
c      ddif=dcmplx( -zm2 , ww )*da*dz
c
      cw=r_one/sqrt(r_one+(gzf(2)/gaf(2))**2)
      sw=sqrt((r_one-cw)*(r_one+cw))
      gn=gaf(2)*sw
      gz3l=gzf(1)*cw
      ga3l=gaf(1)*sw
      c0l=  fo(3)*fi(1)+fo(4)*fi(2)
      c0r=  fo(1)*fi(3)+fo(2)*fi(4)
      c1l=-(fo(3)*fi(2)+fo(4)*fi(1))
      c1r=  fo(1)*fi(4)+fo(2)*fi(3)
      c2l= (fo(3)*fi(2)-fo(4)*fi(1))*c_imag
      c2r=(-fo(1)*fi(4)+fo(2)*fi(3))*c_imag
      c3l= -fo(3)*fi(1)+fo(4)*fi(2)
      c3r=  fo(1)*fi(3)-fo(2)*fi(4)
      csl=(q(0)*c0l-q(1)*c1l-q(2)*c2l-q(3)*c3l)/zm2
      csr=(q(0)*c0r-q(1)*c1r-q(2)*c2r-q(3)*c3r)/zm2
c
      j3(1) =  gz3l*dz*(c0l-csl*q(0))+ga3l*c0l*da
     &       + gn*(c0r*ddif-csr*q(0)*dz)
      j3(2) =  gz3l*dz*(c1l-csl*q(1))+ga3l*c1l*da
     &       + gn*(c1r*ddif-csr*q(1)*dz)
      j3(3) =  gz3l*dz*(c2l-csl*q(2))+ga3l*c2l*da
     &       + gn*(c2r*ddif-csr*q(2)*dz)
      j3(4) =  gz3l*dz*(c3l-csl*q(3))+ga3l*c3l*da
     &       + gn*(c3r*ddif-csr*q(3)*dz)
c
      return
      end subroutine
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JEEXXX(EB,EF,SHLF,CHLF,PHI,NHB,NHF,NSF , JEE)
C
C This subroutine computes an off-shell photon wavefunction emitted from
C the electron or positron beam, with a special care for the small angle
C region.  The momenta are measured in the laboratory frame, where the  
C e- (e+) beam is along the positive (negative) z axis.                 
C                                                                       
C INPUT:                                                                
C       real    EB             : energy (GeV)    of beam  e-/e+         
C       real    EF             : energy (GeV)    of final e-/e+         
C       real    SHLF           : sin(theta/2)    of final e-/e+         
C       real    CHLF           : cos(theta/2)    of final e-/e+         
C       real    PHI            : azimuthal angle of final e-/e+         
C       integer NHB  = -1 or 1 : helicity        of beam  e-/e+         
C       integer NHF  = -1 or 1 : helicity        of final e-/e+         
C       integer NSF  = -1 or 1 : +1 for electron, -1 for positron       
C                                                                       
C OUTPUT:                                                               
C       complex JEE(6)         : off-shell photon          J^mu(<e|A|e>)
C
      implicit none
      COMPLEX*16 JEE(6),COEFF
      REAL*8  CS(2),EB,EF,SHLF,CHLF,PHI,ME,ALPHA,GAL,HI,SF,SFH,X,ME2,Q2,
     &        RFP,RFM,SNP,CSP,RXC,C,S
      INTEGER NHB,NHF,NSF
C
      ME   =0.51099906D-3
      ALPHA=1./128.
      GAL  =SQRT(ALPHA*4.*3.14159265D0)
C
      HI =NHB
      SF =NSF
      SFH=NHB*NSF
      CS((3+NSF)/2)=SHLF
      CS((3-NSF)/2)=CHLF
C CS(1)=CHLF and CS(2)=SHLF for electron
C CS(1)=SHLF and CS(2)=CHLF for positron
      X=EF/EB
      ME2=ME**2
      Q2=-4.*CS(2)**2*(EF*EB-ME2)
     &   +SF*(1.-X)**2/X*(SHLF+CHLF)*(SHLF-CHLF)*ME2
      RFP=(1+NSF)
      RFM=(1-NSF)
      SNP=SIN(PHI)
      CSP=COS(PHI)
C
      IF (NHB.EQ.NHF) THEN
         RXC=2.*X/(1.-X)*CS(1)**2
         COEFF= GAL*2.*EB*SQRT(X)*CS(2)/Q2
     &         *(dCMPLX( RFP )-RFM*dCMPLX( CSP ,-SNP*HI ))*.5
         JEE(1) =  dCMPLX( 0.d0 )
         JEE(2) =  COEFF*dCMPLX( (1.+RXC)*CSP ,-SFH*SNP )
         JEE(3) =  COEFF*dCMPLX( (1.+RXC)*SNP , SFH*CSP )
         JEE(4) =  COEFF*(-SF*RXC/CS(1)*CS(2))
      ELSE
         COEFF= GAL*ME/Q2/SQRT(X)
     &         *(dCMPLX( RFP )+RFM*dCMPLX( CSP , SNP*HI ))*.5*HI
         JEE(1) = -COEFF*(1.+X)*CS(2)*dCMPLX( CSP , SFH*SNP )
         JEE(2) =  COEFF*(1.-X)*CS(1)
         JEE(3) =  JEE(2)*dCMPLX( 0.d0 , SFH )
         JEE(4) =  JEE(1)*SF*(1.-X)/(1.+X)
      ENDIF
C
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.*CHLF*SHLF
C
      JEE(5) = -EB*dCMPLX( 1.-X , SF-X*C )
      JEE(6) =  EB*X*S*dCMPLX( CSP , SNP )
C
      RETURN          
      end subroutine
C
c
c ----------------------------------------------------------------------
c
      subroutine jgggxx(w1,w2,w3,g, jw3w)
c
c this subroutine computes an off-shell w+, w-, w3, z or photon current 
c from the four-point gauge boson coupling, including the contributions 
c of w exchange diagrams.  the vector propagator is given in feynman    
c gauge for a photon and in unitary gauge for w and z bosons.  if one   
c sets wmass=0.0, then the ggg-->g current is given (see sect 2.9.1 of 
c the manual).                                                          
c                                                                       
c input:                                                                
c       complex w1(6)          : first  vector                        w1
c       complex w2(6)          : second vector                        w2
c       complex w3(6)          : third  vector                        w3
c       real    g             : first  coupling constant               
c                                                  (see the table below)
c                                                                       
c output:                                                               
c       complex jw3w(6)        : w current             j^mu(w':w1,w2,w3)
c
      implicit none
      complex*16  w1(6),w2(6),w3(6),jw3w(6)
      complex*16 dw1(0:3),dw2(0:3),dw3(0:3),
     &           jj(0:3),dv,w32,w13
      real*8     p1(0:3),p2(0:3),p3(0:3),q(0:3),g,dg2,q2
c
      real*8 r_zero
      parameter( r_zero=0.0d0 )
c
      jw3w(5) = w1(5)+w2(5)+w3(5)
      jw3w(6) = w1(6)+w2(6)+w3(6)
c
      dw1(0)=dcmplx(w1(1))
      dw1(1)=dcmplx(w1(2))
      dw1(2)=dcmplx(w1(3))
      dw1(3)=dcmplx(w1(4))
      dw2(0)=dcmplx(w2(1))
      dw2(1)=dcmplx(w2(2))
      dw2(2)=dcmplx(w2(3))
      dw2(3)=dcmplx(w2(4))
      dw3(0)=dcmplx(w3(1))
      dw3(1)=dcmplx(w3(2))
      dw3(2)=dcmplx(w3(3))
      dw3(3)=dcmplx(w3(4))
      p1(0)=dble(      w1(5))
      p1(1)=dble(      w1(6))
      p1(2)=dble(dimag(w1(6)))
      p1(3)=dble(dimag(w1(5)))
      p2(0)=dble(      w2(5))
      p2(1)=dble(      w2(6))
      p2(2)=dble(dimag(w2(6)))
      p2(3)=dble(dimag(w2(5)))
      p3(0)=dble(      w3(5))
      p3(1)=dble(      w3(6))
      p3(2)=dble(dimag(w3(6)))
      p3(3)=dble(dimag(w3(5)))
      q(0)=-(p1(0)+p2(0)+p3(0))
      q(1)=-(p1(1)+p2(1)+p3(1))
      q(2)=-(p1(2)+p2(2)+p3(2))
      q(3)=-(p1(3)+p2(3)+p3(3))

      q2 =q(0)**2 -(q(1)**2 +q(2)**2 +q(3)**2)

      dg2=dble(g)*dble(g)
c
      dv = 1.0d0/dcmplx( q2 )

c  for the running width, use below instead of the above dv.
c      dv = 1.0d0/dcmplx( q2 -mv2 , dmax1(dwv*q2/dmv,0.d0) )
c
      w32=dw3(0)*dw2(0)-dw3(1)*dw2(1)-dw3(2)*dw2(2)-dw3(3)*dw2(3)
c
c     
      w13=dw1(0)*dw3(0)-dw1(1)*dw3(1)-dw1(2)*dw3(2)-dw1(3)*dw3(3)
c     
      jj(0)=dg2*( dw1(0)*w32 - dw2(0)*w13 )
      jj(1)=dg2*( dw1(1)*w32 - dw2(1)*w13 )
      jj(2)=dg2*( dw1(2)*w32 - dw2(2)*w13 )
      jj(3)=dg2*( dw1(3)*w32 - dw2(3)*w13 )
c     
      jw3w(1) = dcmplx( jj(0)*dv )
      jw3w(2) = dcmplx( jj(1)*dv )
      jw3w(3) = dcmplx( jj(2)*dv )
      jw3w(4) = dcmplx( jj(3)*dv )
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine jggxxx(v1,v2,g, jvv)
c
c this subroutine computes an off-shell vector current from the three-  
c point gauge boson coupling.  the vector propagator is given in feynman
c gauge for a massless vector and in unitary gauge for a massive vector.
c                                                                       
c input:                                                                
c       complex v1(6)          : first  vector                        v1
c       complex v2(6)          : second vector                        v2
c       real    g              : coupling constant (see the table below)
c                                                                       
c output:                                                               
c       complex jvv(6)         : vector current            j^mu(v:v1,v2)
c
      complex*16 v1(6),v2(6),jvv(6),j12(0:3),
     &        sv1,sv2,v12
      real*8    p1(0:3),p2(0:3),q(0:3),g,gs,s
c
      real*8 r_zero
      parameter( r_zero=0.0d0 )
c
      jvv(5) = v1(5)+v2(5)
      jvv(6) = v1(6)+v2(6)
c
      p1(0)=dble( v1(5))
      p1(1)=dble( v1(6))
      p1(2)=dimag(v1(6))
      p1(3)=dimag(v1(5))
      p2(0)=dble( v2(5))
      p2(1)=dble( v2(6))
      p2(2)=dimag(v2(6))
      p2(3)=dimag(v2(5))
      q(0)=-dble( jvv(5))
      q(1)=-dble( jvv(6))
      q(2)=-dimag(jvv(6))
      q(3)=-dimag(jvv(5))
      s=q(0)**2-(q(1)**2+q(2)**2+q(3)**2)
c
      v12=v1(1)*v2(1)-v1(2)*v2(2)-v1(3)*v2(3)-v1(4)*v2(4)
      sv1= (p2(0)-q(0))*v1(1) -(p2(1)-q(1))*v1(2)
     &    -(p2(2)-q(2))*v1(3) -(p2(3)-q(3))*v1(4)
      sv2=-(p1(0)-q(0))*v2(1) +(p1(1)-q(1))*v2(2)
     &    +(p1(2)-q(2))*v2(3) +(p1(3)-q(3))*v2(4)
      j12(0)=(p1(0)-p2(0))*v12 +sv1*v2(1) +sv2*v1(1)
      j12(1)=(p1(1)-p2(1))*v12 +sv1*v2(2) +sv2*v1(2)
      j12(2)=(p1(2)-p2(2))*v12 +sv1*v2(3) +sv2*v1(3)
      j12(3)=(p1(3)-p2(3))*v12 +sv1*v2(4) +sv2*v1(4)
c
      gs=-g/s
c
      jvv(1) = gs*j12(0)
      jvv(2) = gs*j12(1)
      jvv(3) = gs*j12(2)
      jvv(4) = gs*j12(3)
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine jioxxx(fi,fo,g,vmass,vwidth , jio)
c
c this subroutine computes an off-shell vector current from an external 
c fermion pair.  the vector boson propagator is given in feynman gauge  
c for a massless vector and in unitary gauge for a massive vector.      
c                                                                       
c input:                                                                
c       complex fi(6)          : flow-in  fermion                   |fi>
c       complex fo(6)          : flow-out fermion                   <fo|
c       real    g(2)           : coupling constants                  gvf
c       real    vmass          : mass  of output vector v               
c       real    vwidth         : width of output vector v               
c                                                                       
c output:                                                               
c       complex jio(6)         : vector current          j^mu(<fo|v|fi>)
c
      complex*16 fi(6),fo(6),jio(6),c0,c1,c2,c3,cs,d
      real*8    g(2),q(0:3),vmass,vwidth,q2,vm2,dd
c
      real*8 r_zero, r_one
      parameter( r_zero=0.0d0, r_one=1.0d0 )
      complex*16 c_imag
      c_imag=dcmplx( r_zero, r_one )
c
      jio(5) = fo(5)-fi(5)
      jio(6) = fo(6)-fi(6)
c
      q(0)=dble( jio(5))
      q(1)=dble( jio(6))
      q(2)=dimag(jio(6))
      q(3)=dimag(jio(5))
      q2=q(0)**2-(q(1)**2+q(2)**2+q(3)**2)
      vm2=vmass**2
c
      if (vmass.ne.r_zero) then
c
         d=r_one/dcmplx( q2-vm2 , max(sign( vmass*vwidth ,q2),r_zero) )
c  for the running width, use below instead of the above d.
c      d=r_one/dcmplx( q2-vm2 , max( vwidth*q2/vmass ,r_zero) )
c
         if (g(2).ne.r_zero) then
c
            c0=  g(1)*( fo(3)*fi(1)+fo(4)*fi(2))
     &          +g(2)*( fo(1)*fi(3)+fo(2)*fi(4))
            c1= -g(1)*( fo(3)*fi(2)+fo(4)*fi(1))
     &          +g(2)*( fo(1)*fi(4)+fo(2)*fi(3))
            c2=( g(1)*( fo(3)*fi(2)-fo(4)*fi(1)) 
     &          +g(2)*(-fo(1)*fi(4)+fo(2)*fi(3)))*c_imag
            c3=  g(1)*(-fo(3)*fi(1)+fo(4)*fi(2))
     &          +g(2)*( fo(1)*fi(3)-fo(2)*fi(4))
         else
c
            d=d*g(1)
            c0=  fo(3)*fi(1)+fo(4)*fi(2)
            c1= -fo(3)*fi(2)-fo(4)*fi(1)
            c2=( fo(3)*fi(2)-fo(4)*fi(1))*c_imag
            c3= -fo(3)*fi(1)+fo(4)*fi(2)
         end if
c
         cs=(q(0)*c0-q(1)*c1-q(2)*c2-q(3)*c3)/vm2
c
         jio(1) = (c0-cs*q(0))*d
         jio(2) = (c1-cs*q(1))*d
         jio(3) = (c2-cs*q(2))*d
         jio(4) = (c3-cs*q(3))*d
c
      else
         dd=r_one/q2
c
         if (g(2).ne.r_zero) then
            jio(1) = ( g(1)*( fo(3)*fi(1)+fo(4)*fi(2))
     &                +g(2)*( fo(1)*fi(3)+fo(2)*fi(4)) )*dd
            jio(2) = (-g(1)*( fo(3)*fi(2)+fo(4)*fi(1))
     &                +g(2)*( fo(1)*fi(4)+fo(2)*fi(3)) )*dd
            jio(3) = ( g(1)*( fo(3)*fi(2)-fo(4)*fi(1))
     &                +g(2)*(-fo(1)*fi(4)+fo(2)*fi(3)))
     $           *dcmplx(r_zero,dd)
            jio(4) = ( g(1)*(-fo(3)*fi(1)+fo(4)*fi(2))
     &                +g(2)*( fo(1)*fi(3)-fo(2)*fi(4)) )*dd
c
         else
            dd=dd*g(1)
c
            jio(1) =  ( fo(3)*fi(1)+fo(4)*fi(2))*dd
            jio(2) = -( fo(3)*fi(2)+fo(4)*fi(1))*dd
            jio(3) =  ( fo(3)*fi(2)-fo(4)*fi(1))*dcmplx(r_zero,dd)
            jio(4) =  (-fo(3)*fi(1)+fo(4)*fi(2))*dd
         end if
      end if
c
      return
      end subroutine
C ----------------------------------------------------------------------
C
      SUBROUTINE JSSXXX(S1,S2,G,VMASS,VWIDTH , JSS)
C
C This subroutine computes an off-shell vector current from the vector- 
C scalar-scalar coupling.  The coupling is absent in the minimal SM in  
C unitary gauge.  The propagator is given in Feynman gauge for a        
C massless vector and in unitary gauge for a massive vector.            
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant (S1 charge)          
C       real    VMASS          : mass  of OUTPUT vector V               
C       real    VWIDTH         : width of OUTPUT vector V               
C                                                                       
C Examples of the coupling constant G for SUSY particles are as follows:
C   -----------------------------------------------------------         
C   |    S1    | (Q,I3) of S1  ||   V=A   |   V=Z   |   V=W   |         
C   -----------------------------------------------------------         
C   | nu~_L    | (  0  , +1/2) ||   ---   |  GZN(1) |  GWF(1) |         
C   | e~_L     | ( -1  , -1/2) ||  GAL(1) |  GZL(1) |  GWF(1) |         
C   | u~_L     | (+2/3 , +1/2) ||  GAU(1) |  GZU(1) |  GWF(1) |         
C   | d~_L     | (-1/3 , -1/2) ||  GAD(1) |  GZD(1) |  GWF(1) |         
C   -----------------------------------------------------------         
C   | e~_R-bar | ( +1  ,  0  ) || -GAL(2) | -GZL(2) | -GWF(2) |         
C   | u~_R-bar | (-2/3 ,  0  ) || -GAU(2) | -GZU(2) | -GWF(2) |         
C   | d~_R-bar | (+1/3 ,  0  ) || -GAD(2) | -GZD(2) | -GWF(2) |         
C   -----------------------------------------------------------         
C where the S1 charge is defined by the flowing-OUT quantum number.     
C                                                                       
C OUTPUT:                                                               
C       complex JSS(6)         : vector current            J^mu(V:S1,S2)
C
      implicit none
      COMPLEX*16 S1(3),S2(3),JSS(6),DG,ADG
      REAL*8  PP(0:3),PA(0:3),Q(0:3),G,VMASS,VWIDTH,Q2,VM2,MP2,MA2,M2D
C
      JSS(5) = S1(2)+S2(2)
      JSS(6) = S1(3)+S2(3)
C
      Q(0)=dble( JSS(5))
      Q(1)=dble( JSS(6))
      Q(2)=dIMAG(JSS(6))
      Q(3)=dIMAG(JSS(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      IF (VMASS.EQ.0.) GOTO 10
C
      DG=G/dCMPLX( Q2-VM2, MAX(SIGN( VMASS*VWIDTH ,Q2),0.d0))
C  For the running width, use below instead of the above DG.
C      DG=G/dCMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      ADG=DG*S1(1)*S2(1)
C
      PP(0)=dble( S1(2))
      PP(1)=dble( S1(3))
      PP(2)=dIMAG(S1(3))
      PP(3)=dIMAG(S1(2))
      PA(0)=dble( S2(2))
      PA(1)=dble( S2(3))
      PA(2)=dIMAG(S2(3))
      PA(3)=dIMAG(S2(2))
      MP2=PP(0)**2-(PP(1)**2+PP(2)**2+PP(3)**2)
      MA2=PA(0)**2-(PA(1)**2+PA(2)**2+PA(3)**2)
      M2D=MP2-MA2
C
      JSS(1) = ADG*( (PP(0)-PA(0)) - Q(0)*M2D/VM2)
      JSS(2) = ADG*( (PP(1)-PA(1)) - Q(1)*M2D/VM2)
      JSS(3) = ADG*( (PP(2)-PA(2)) - Q(2)*M2D/VM2)
      JSS(4) = ADG*( (PP(3)-PA(3)) - Q(3)*M2D/VM2)
C
      RETURN
C
  10  ADG=G*S1(1)*S2(1)/Q2
C
      JSS(1) = ADG*dble( S1(2)-S2(2))
      JSS(2) = ADG*dble( S1(3)-S2(3))
      JSS(3) = ADG*dIMAG(S1(3)-S2(3))
      JSS(4) = ADG*dIMAG(S1(2)-S2(2))
C
      RETURN
      end subroutine
C
c
c ----------------------------------------------------------------------
c
      subroutine jtioxx(fi,fo,g , jio)
c
c this subroutine computes an off-shell vector current from an external 
c fermion pair.  the vector boson propagator is not included in this
c routine.
c                                                                       
c input:                                                                
c       complex fi(6)          : flow-in  fermion                   |fi>
c       complex fo(6)          : flow-out fermion                   <fo|
c       real    g(2)           : coupling constants                  gvf
c                                                                       
c output:                                                               
c       complex jio(6)         : vector current          j^mu(<fo|v|fi>)
c
      complex*16 fi(6),fo(6),jio(6)
      real*8    g(2)
c
      real*8 r_zero, r_one
      parameter( r_zero=0.0d0, r_one=1.0d0 )
      complex*16 c_imag
      c_imag=dcmplx( r_zero, r_one )
c
      jio(5) = fo(5)-fi(5)
      jio(6) = fo(6)-fi(6)
c
      if ( g(2) .ne. r_zero ) then
         jio(1) = ( g(1)*( fo(3)*fi(1)+fo(4)*fi(2))
     &             +g(2)*( fo(1)*fi(3)+fo(2)*fi(4)) )
         jio(2) = (-g(1)*( fo(3)*fi(2)+fo(4)*fi(1))
     &             +g(2)*( fo(1)*fi(4)+fo(2)*fi(3)) )
         jio(3) = ( g(1)*( fo(3)*fi(2)-fo(4)*fi(1))
     &             +g(2)*(-fo(1)*fi(4)+fo(2)*fi(3)) )*c_imag
         jio(4) = ( g(1)*(-fo(3)*fi(1)+fo(4)*fi(2))
     &             +g(2)*( fo(1)*fi(3)-fo(2)*fi(4)) )
c
      else
         jio(1) =  ( fo(3)*fi(1)+fo(4)*fi(2))*g(1)
         jio(2) = -( fo(3)*fi(2)+fo(4)*fi(1))*g(1)
         jio(3) =  ( fo(3)*fi(2)-fo(4)*fi(1))*dcmplx(r_zero,g(1))
         jio(4) =  (-fo(3)*fi(1)+fo(4)*fi(2))*g(1)
      end if
c
      return
      end subroutine
C ----------------------------------------------------------------------
C
      SUBROUTINE JVSSXX(VC,S1,S2,G,VMASS,VWIDTH , JVSS)
C
C This subroutine computes an off-shell vector current from the vector- 
C vector-scalar-scalar coupling.  The vector propagator is given in     
C Feynman gauge for a massless vector and in unitary gauge for a massive
C vector.                                                               
C                                                                       
C INPUT:                                                                
C       complex VC(6)          : input  vector                        V 
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant                 GVVHH
C       real    VMASS          : mass  of OUTPUT vector V'              
C       real    VWIDTH         : width of OUTPUT vector V'              
C                                                                       
C OUTPUT:                                                               
C       complex JVSS(6)        : vector current         J^mu(V':V,S1,S2)
C
      implicit none
      COMPLEX*16 VC(6),S1(3),S2(3),JVSS(6),DG
      REAL*8    Q(0:3),G,VMASS,VWIDTH,Q2,VK,VM2
C
      JVSS(5) = VC(5)+S1(2)+S2(2)
      JVSS(6) = VC(6)+S1(3)+S2(3)
C
      Q(0)=dble( JVSS(5))
      Q(1)=dble( JVSS(6))
      Q(2)=dIMAG(JVSS(6))
      Q(3)=dIMAG(JVSS(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      IF (VMASS.EQ.0.) GOTO 10
C
      DG=G*S1(1)*S2(1)/dCMPLX( Q2-VM2,MAX(SIGN( VMASS*VWIDTH,Q2),0.d0))
C  For the running width, use below instead of the above DG.
C      DG=G*S1(1)*S2(1)/CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.))
C
      VK=(Q(0)*VC(1)-Q(1)*VC(2)-Q(2)*VC(3)-Q(3)*VC(4))/VM2
C
      JVSS(1) = DG*(VC(1)-VK*Q(0))
      JVSS(2) = DG*(VC(2)-VK*Q(1))
      JVSS(3) = DG*(VC(3)-VK*Q(2))
      JVSS(4) = DG*(VC(4)-VK*Q(3))
C
      RETURN
C
  10  DG= G*S1(1)*S2(1)/Q2
C
      JVSS(1) = DG*VC(1)
      JVSS(2) = DG*VC(2)
      JVSS(3) = DG*VC(3)
      JVSS(4) = DG*VC(4)
C
      RETURN
      end subroutine
C
c
c ----------------------------------------------------------------------
c
      subroutine jvsxxx(vc,sc,g,vmass,vwidth , jvs)
      implicit real*8(a-h,o-z)
c
c this subroutine computes an off-shell vector current from the vector- 
c vector-scalar coupling.  the vector propagator is given in feynman    
c gauge for a massless vector and in unitary gauge for a massive vector.
c                                                                       
c input:                                                                
c       complex vc(6)          : input vector                          v
c       complex sc(3)          : input scalar                          s
c       real    g              : coupling constant                  gvvh
c       real    vmass          : mass  of output vector v'              
c       real    vwidth         : width of output vector v'              
c                                                                       
c output:                                                               
c       complex jvs(6)         : vector current             j^mu(v':v,s)
c
      complex*16 vc(6),sc(3),jvs(6),dg,vk
      real*8    q(0:3),vmass,vwidth,q2,vm2,g
c
      jvs(5) = vc(5)+sc(2)
      jvs(6) = vc(6)+sc(3)
c
      q(0)=dble( jvs(5))
      q(1)=dble( jvs(6))
      q(2)=dimag(jvs(6))
      q(3)=dimag(jvs(5))
      q2=q(0)**2-(q(1)**2+q(2)**2+q(3)**2)
      vm2=vmass**2
c
      if (vmass.eq.0.) goto 10
c
      dg=g*sc(1)/dcmplx( q2-vm2 , max(dsign( vmass*vwidth ,q2),0.d0) )
c  for the running width, use below instead of the above dg.
c      dg=g*sc(1)/dcmplx( q2-vm2 , max( vwidth*q2/vmass ,0.) )
c
      vk=(-q(0)*vc(1)+q(1)*vc(2)+q(2)*vc(3)+q(3)*vc(4))/vm2
c
      jvs(1) = dg*(q(0)*vk+vc(1))
      jvs(2) = dg*(q(1)*vk+vc(2))
      jvs(3) = dg*(q(2)*vk+vc(3))
      jvs(4) = dg*(q(3)*vk+vc(4))
c
      return
c
  10  dg=g*sc(1)/q2
c
      jvs(1) = dg*vc(1)
      jvs(2) = dg*vc(2)
      jvs(3) = dg*vc(3)
      jvs(4) = dg*vc(4)
c
      return
      end subroutine


c
c ----------------------------------------------------------------------
c
      subroutine jvvxxx(v1,v2,g,vmass,vwidth , jvv)
c
c this subroutine computes an off-shell vector current from the three-  
c point gauge boson coupling.  the vector propagator is given in feynman
c gauge for a massless vector and in unitary gauge for a massive vector.
c                                                                       
c input:                                                                
c       complex v1(6)          : first  vector                        v1
c       complex v2(6)          : second vector                        v2
c       real    g              : coupling constant (see the table below)
c       real    vmass          : mass  of output vector v               
c       real    vwidth         : width of output vector v               
c                                                                       
c the possible sets of the inputs are as follows:                       
c    ------------------------------------------------------------------ 
c    |   v1   |   v2   |  jvv   |      g       |   vmass  |  vwidth   | 
c    ------------------------------------------------------------------ 
c    |   w-   |   w+   |  a/z   |  gwwa/gwwz   | 0./zmass | 0./zwidth | 
c    | w3/a/z |   w-   |  w+    | gw/gwwa/gwwz |   wmass  |  wwidth   | 
c    |   w+   | w3/a/z |  w-    | gw/gwwa/gwwz |   wmass  |  wwidth   | 
c    ------------------------------------------------------------------ 
c where all the bosons are defined by the flowing-out quantum number.   
c                                                                       
c output:                                                               
c       complex jvv(6)         : vector current            j^mu(v:v1,v2)
c
      complex*16 v1(6),v2(6),jvv(6),j12(0:3),js,dg,
     &        sv1,sv2,s11,s12,s21,s22,v12
      real*8    p1(0:3),p2(0:3),q(0:3),g,vmass,vwidth,gs,s,vm2,m1,m2
c
      real*8 r_zero
      parameter( r_zero=0.0d0 )
c
      jvv(5) = v1(5)+v2(5)
      jvv(6) = v1(6)+v2(6)
c
      p1(0)=dble( v1(5))
      p1(1)=dble( v1(6))
      p1(2)=dimag(v1(6))
      p1(3)=dimag(v1(5))
      p2(0)=dble( v2(5))
      p2(1)=dble( v2(6))
      p2(2)=dimag(v2(6))
      p2(3)=dimag(v2(5))
      q(0)=-dble( jvv(5))
      q(1)=-dble( jvv(6))
      q(2)=-dimag(jvv(6))
      q(3)=-dimag(jvv(5))
      s=q(0)**2-(q(1)**2+q(2)**2+q(3)**2)
c
      v12=v1(1)*v2(1)-v1(2)*v2(2)-v1(3)*v2(3)-v1(4)*v2(4)
      sv1= (p2(0)-q(0))*v1(1) -(p2(1)-q(1))*v1(2)
     &    -(p2(2)-q(2))*v1(3) -(p2(3)-q(3))*v1(4)
      sv2=-(p1(0)-q(0))*v2(1) +(p1(1)-q(1))*v2(2)
     &    +(p1(2)-q(2))*v2(3) +(p1(3)-q(3))*v2(4)
      j12(0)=(p1(0)-p2(0))*v12 +sv1*v2(1) +sv2*v1(1)
      j12(1)=(p1(1)-p2(1))*v12 +sv1*v2(2) +sv2*v1(2)
      j12(2)=(p1(2)-p2(2))*v12 +sv1*v2(3) +sv2*v1(3)
      j12(3)=(p1(3)-p2(3))*v12 +sv1*v2(4) +sv2*v1(4)
c
      if ( vmass .ne. r_zero ) then
         vm2=vmass**2
         m1=p1(0)**2-(p1(1)**2+p1(2)**2+p1(3)**2)
         m2=p2(0)**2-(p2(1)**2+p2(2)**2+p2(3)**2)
         s11=p1(0)*v1(1)-p1(1)*v1(2)-p1(2)*v1(3)-p1(3)*v1(4)
         s12=p1(0)*v2(1)-p1(1)*v2(2)-p1(2)*v2(3)-p1(3)*v2(4)
         s21=p2(0)*v1(1)-p2(1)*v1(2)-p2(2)*v1(3)-p2(3)*v1(4)
         s22=p2(0)*v2(1)-p2(1)*v2(2)-p2(2)*v2(3)-p2(3)*v2(4)
         js=(v12*(-m1+m2) +s11*s12 -s21*s22)/vm2
c
         dg=-g/dcmplx( s-vm2 , max(sign( vmass*vwidth ,s),r_zero) )
c
c  for the running width, use below instead of the above dg.
c         dg=-g/dcmplx( s-vm2 , max( vwidth*s/vmass ,r_zero) )
c
         jvv(1) = dg*(j12(0)-q(0)*js)
         jvv(2) = dg*(j12(1)-q(1)*js)
         jvv(3) = dg*(j12(2)-q(2)*js)
         jvv(4) = dg*(j12(3)-q(3)*js)
c
      else
         gs=-g/s
c
         jvv(1) = gs*j12(0)
         jvv(2) = gs*j12(1)
         jvv(3) = gs*j12(2)
         jvv(4) = gs*j12(3)
      end if
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine jw3wxx(w1,w2,w3,g1,g2,wmass,wwidth,vmass,vwidth , jw3w)
c
c this subroutine computes an off-shell w+, w-, w3, z or photon current 
c from the four-point gauge boson coupling, including the contributions 
c of w exchange diagrams.  the vector propagator is given in feynman    
c gauge for a photon and in unitary gauge for w and z bosons.  if one   
c sets wmass=0.0, then the ggg-->g current is given (see sect 2.9.1 of 
c the manual).                                                          
c                                                                       
c input:                                                                
c       complex w1(6)          : first  vector                        w1
c       complex w2(6)          : second vector                        w2
c       complex w3(6)          : third  vector                        w3
c       real    g1             : first  coupling constant               
c       real    g2             : second coupling constant               
c                                                  (see the table below)
c       real    wmass          : mass  of internal w                    
c       real    wwidth         : width of internal w                    
c       real    vmass          : mass  of output w'                     
c       real    vwidth         : width of output w'                     
c                                                                       
c the possible sets of the inputs are as follows:                       
c   ------------------------------------------------------------------- 
c   |  w1  |  w2  |  w3  | g1 | g2 |wmass|wwidth|vmass|vwidth || jw3w | 
c   ------------------------------------------------------------------- 
c   |  w-  |  w3  |  w+  | gw |gwwz|wmass|wwidth|zmass|zwidth ||  z   | 
c   |  w-  |  w3  |  w+  | gw |gwwa|wmass|wwidth|  0. |  0.   ||  a   | 
c   |  w-  |  z   |  w+  |gwwz|gwwz|wmass|wwidth|zmass|zwidth ||  z   | 
c   |  w-  |  z   |  w+  |gwwz|gwwa|wmass|wwidth|  0. |  0.   ||  a   | 
c   |  w-  |  a   |  w+  |gwwa|gwwz|wmass|wwidth|zmass|zwidth ||  z   | 
c   |  w-  |  a   |  w+  |gwwa|gwwa|wmass|wwidth|  0. |  0.   ||  a   | 
c   ------------------------------------------------------------------- 
c   |  w3  |  w-  |  w3  | gw | gw |wmass|wwidth|wmass|wwidth ||  w+  | 
c   |  w3  |  w+  |  w3  | gw | gw |wmass|wwidth|wmass|wwidth ||  w-  | 
c   |  w3  |  w-  |  z   | gw |gwwz|wmass|wwidth|wmass|wwidth ||  w+  | 
c   |  w3  |  w+  |  z   | gw |gwwz|wmass|wwidth|wmass|wwidth ||  w-  | 
c   |  w3  |  w-  |  a   | gw |gwwa|wmass|wwidth|wmass|wwidth ||  w+  | 
c   |  w3  |  w+  |  a   | gw |gwwa|wmass|wwidth|wmass|wwidth ||  w-  | 
c   |  z   |  w-  |  z   |gwwz|gwwz|wmass|wwidth|wmass|wwidth ||  w+  | 
c   |  z   |  w+  |  z   |gwwz|gwwz|wmass|wwidth|wmass|wwidth ||  w-  | 
c   |  z   |  w-  |  a   |gwwz|gwwa|wmass|wwidth|wmass|wwidth ||  w+  | 
c   |  z   |  w+  |  a   |gwwz|gwwa|wmass|wwidth|wmass|wwidth ||  w-  | 
c   |  a   |  w-  |  a   |gwwa|gwwa|wmass|wwidth|wmass|wwidth ||  w+  | 
c   |  a   |  w+  |  a   |gwwa|gwwa|wmass|wwidth|wmass|wwidth ||  w-  | 
c   ------------------------------------------------------------------- 
c where all the bosons are defined by the flowing-out quantum number.   
c                                                                       
c output:                                                               
c       complex jw3w(6)        : w current             j^mu(w':w1,w2,w3)
c
      complex*16  w1(6),w2(6),w3(6),jw3w(6)
      complex*16 dw1(0:3),dw2(0:3),dw3(0:3),
     &           jj(0:3),j4(0:3),
     &           dv,w12,w32,w13,
     &           jq
      real*8     g1,g2,wmass,wwidth,vmass,vwidth
      real*8     p1(0:3),p2(0:3),p3(0:3),q(0:3),
     &           dg2,dmv,dwv,mv2,q2
c
      real*8 r_zero
      parameter( r_zero=0.0d0 )
c
      jw3w(5) = w1(5)+w2(5)+w3(5)
      jw3w(6) = w1(6)+w2(6)+w3(6)
c
      dw1(0)=dcmplx(w1(1))
      dw1(1)=dcmplx(w1(2))
      dw1(2)=dcmplx(w1(3))
      dw1(3)=dcmplx(w1(4))
      dw2(0)=dcmplx(w2(1))
      dw2(1)=dcmplx(w2(2))
      dw2(2)=dcmplx(w2(3))
      dw2(3)=dcmplx(w2(4))
      dw3(0)=dcmplx(w3(1))
      dw3(1)=dcmplx(w3(2))
      dw3(2)=dcmplx(w3(3))
      dw3(3)=dcmplx(w3(4))
      p1(0)=dble(      w1(5))
      p1(1)=dble(      w1(6))
      p1(2)=dble(dimag(w1(6)))
      p1(3)=dble(dimag(w1(5)))
      p2(0)=dble(      w2(5))
      p2(1)=dble(      w2(6))
      p2(2)=dble(dimag(w2(6)))
      p2(3)=dble(dimag(w2(5)))
      p3(0)=dble(      w3(5))
      p3(1)=dble(      w3(6))
      p3(2)=dble(dimag(w3(6)))
      p3(3)=dble(dimag(w3(5)))
      q(0)=-(p1(0)+p2(0)+p3(0))
      q(1)=-(p1(1)+p2(1)+p3(1))
      q(2)=-(p1(2)+p2(2)+p3(2))
      q(3)=-(p1(3)+p2(3)+p3(3))


      q2 =q(0)**2 -(q(1)**2 +q(2)**2 +q(3)**2)
      dg2=dble(g1)*dble(g2)
      dmv=dble(vmass)
      dwv=dble(vwidth)
      mv2=dmv**2
      if (vmass.eq. r_zero) then
      dv = 1.0d0/dcmplx( q2 )
      else
      dv = 1.0d0/dcmplx( q2 -mv2 , dmax1(dsign(dmv*dwv,q2 ),0.d0) )
      endif
c  for the running width, use below instead of the above dv.
c      dv = 1.0d0/dcmplx( q2 -mv2 , dmax1(dwv*q2/dmv,0.d0) )
c
      w12=dw1(0)*dw2(0)-dw1(1)*dw2(1)-dw1(2)*dw2(2)-dw1(3)*dw2(3)
      w32=dw3(0)*dw2(0)-dw3(1)*dw2(1)-dw3(2)*dw2(2)-dw3(3)*dw2(3)
c
      if ( wmass .ne. r_zero ) then
         w13=dw1(0)*dw3(0)-dw1(1)*dw3(1)-dw1(2)*dw3(2)-dw1(3)*dw3(3)
c
         j4(0)=dg2*( dw1(0)*w32 + dw3(0)*w12 - 2.d0*dw2(0)*w13 )
         j4(1)=dg2*( dw1(1)*w32 + dw3(1)*w12 - 2.d0*dw2(1)*w13 )
         j4(2)=dg2*( dw1(2)*w32 + dw3(2)*w12 - 2.d0*dw2(2)*w13 )
         j4(3)=dg2*( dw1(3)*w32 + dw3(3)*w12 - 2.d0*dw2(3)*w13 )
c
         jj(0)=j4(0)
         jj(1)=j4(1)
         jj(2)=j4(2)
         jj(3)=j4(3)

      else
c
         w12=dw1(0)*dw2(0)-dw1(1)*dw2(1)-dw1(2)*dw2(2)-dw1(3)*dw2(3)
         w32=dw3(0)*dw2(0)-dw3(1)*dw2(1)-dw3(2)*dw2(2)-dw3(3)*dw2(3)
         w13=dw1(0)*dw3(0)-dw1(1)*dw3(1)-dw1(2)*dw3(2)-dw1(3)*dw3(3)
c
         j4(0)=dg2*( dw1(0)*w32 - dw2(0)*w13 )
         j4(1)=dg2*( dw1(1)*w32 - dw2(1)*w13 )
         j4(2)=dg2*( dw1(2)*w32 - dw2(2)*w13 )
         j4(3)=dg2*( dw1(3)*w32 - dw2(3)*w13 )
c
         jj(0)=j4(0)
         jj(1)=j4(1)
         jj(2)=j4(2)
         jj(3)=j4(3)

      end if
c
      if ( vmass .ne. r_zero ) then
c
         jq=(jj(0)*q(0)-jj(1)*q(1)-jj(2)*q(2)-jj(3)*q(3))/mv2
c
         jw3w(1) = dcmplx( (jj(0)-jq*q(0))*dv )
         jw3w(2) = dcmplx( (jj(1)-jq*q(1))*dv )
         jw3w(3) = dcmplx( (jj(2)-jq*q(2))*dv )
         jw3w(4) = dcmplx( (jj(3)-jq*q(3))*dv )
c
      else
c
         jw3w(1) = dcmplx( jj(0)*dv )
         jw3w(2) = dcmplx( jj(1)*dv )
         jw3w(3) = dcmplx( jj(2)*dv )
         jw3w(4) = dcmplx( jj(3)*dv )
      end if
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine jwwwxx(w1,w2,w3,gwwa,gwwz,zmass,zwidth,wmass,wwidth ,
     &                  jwww)
c
c this subroutine computes an off-shell w+/w- current from the four-    
c point gauge boson coupling, including the contributions of photon and 
c z exchanges.  the vector propagators for the output w and the internal
c z bosons are given in unitary gauge, and that of the internal photon  
c is given in feynman gauge.                                            
c                                                                       
c input:                                                                
c       complex w1(6)          : first  vector                        w1
c       complex w2(6)          : second vector                        w2
c       complex w3(6)          : third  vector                        w3
c       real    gwwa           : coupling constant of w and a       gwwa
c       real    gwwz           : coupling constant of w and z       gwwz
c       real    zmass          : mass  of internal z                    
c       real    zwidth         : width of internal z                    
c       real    wmass          : mass  of output w                      
c       real    wwidth         : width of output w                      
c                                                                       
c the possible sets of the inputs are as follows:                       
c   ------------------------------------------------------------------- 
c   |  w1  |  w2  |  w3  |gwwa|gwwz|zmass|zwidth|wmass|wwidth || jwww | 
c   ------------------------------------------------------------------- 
c   |  w-  |  w+  |  w-  |gwwa|gwwz|zmass|zwidth|wmass|wwidth ||  w+  | 
c   |  w+  |  w-  |  w+  |gwwa|gwwz|zmass|zwidth|wmass|wwidth ||  w-  | 
c   ------------------------------------------------------------------- 
c where all the bosons are defined by the flowing-out quantum number.   
c                                                                       
c output:                                                               
c       complex jwww(6)        : w current             j^mu(w':w1,w2,w3)
c
      complex*16  w1(6),w2(6),w3(6),jwww(6)
      complex*16 dw1(0:3),dw2(0:3),dw3(0:3),
     &           jj(0:3),js(0:3),jt(0:3),j4(0:3),
     &           jt12(0:3),jt32(0:3),j12(0:3),j32(0:3),
     &           dzs,dzt,dw,w12,w32,w13,p1w2,p2w1,p3w2,p2w3,
     &           jk12,jk32,jsw3,jtw1,p3js,ksw3,p1jt,ktw1,jq
      real*8     gwwa,gwwz,zmass,zwidth,wmass,wwidth
      real*8     p1(0:3),p2(0:3),p3(0:3),q(0:3),ks(0:3),kt(0:3),
     &           dgwwa2,dgwwz2,dgw2,dmz,dwz,dmw,dww,mz2,mw2,q2,ks2,kt2,
     &           das,dat
c
      jwww(5) = w1(5)+w2(5)+w3(5)
      jwww(6) = w1(6)+w2(6)+w3(6)
c
      dw1(0)=dcmplx(w1(1))
      dw1(1)=dcmplx(w1(2))
      dw1(2)=dcmplx(w1(3))
      dw1(3)=dcmplx(w1(4))
      dw2(0)=dcmplx(w2(1))
      dw2(1)=dcmplx(w2(2))
      dw2(2)=dcmplx(w2(3))
      dw2(3)=dcmplx(w2(4))
      dw3(0)=dcmplx(w3(1))
      dw3(1)=dcmplx(w3(2))
      dw3(2)=dcmplx(w3(3))
      dw3(3)=dcmplx(w3(4))
      p1(0)=dble(      w1(5))
      p1(1)=dble(      w1(6))
      p1(2)=dble(dimag(w1(6)))
      p1(3)=dble(dimag(w1(5)))
      p2(0)=dble(      w2(5))
      p2(1)=dble(      w2(6))
      p2(2)=dble(dimag(w2(6)))
      p2(3)=dble(dimag(w2(5)))
      p3(0)=dble(      w3(5))
      p3(1)=dble(      w3(6))
      p3(2)=dble(dimag(w3(6)))
      p3(3)=dble(dimag(w3(5)))
      q(0)=-(p1(0)+p2(0)+p3(0))
      q(1)=-(p1(1)+p2(1)+p3(1))
      q(2)=-(p1(2)+p2(2)+p3(2))
      q(3)=-(p1(3)+p2(3)+p3(3))
      ks(0)=p1(0)+p2(0)
      ks(1)=p1(1)+p2(1)
      ks(2)=p1(2)+p2(2)
      ks(3)=p1(3)+p2(3)
      kt(0)=p2(0)+p3(0)
      kt(1)=p2(1)+p3(1)
      kt(2)=p2(2)+p3(2)
      kt(3)=p2(3)+p3(3)
      q2 =q(0)**2 -(q(1)**2 +q(2)**2 +q(3)**2)
      ks2=ks(0)**2-(ks(1)**2+ks(2)**2+ks(3)**2)
      kt2=kt(0)**2-(kt(1)**2+kt(2)**2+kt(3)**2)
      dgwwa2=dble(gwwa)**2
      dgwwz2=dble(gwwz)**2
      dgw2  =dgwwa2+dgwwz2
      dmz=dble(zmass)
      dwz=dble(zwidth)
      dmw=dble(wmass)
      dww=dble(wwidth)
      mz2=dmz**2
      mw2=dmw**2
c
      das=-dgwwa2/ks2
      dat=-dgwwa2/kt2
      dzs=-dgwwz2/dcmplx( ks2-mz2 , dmax1(dsign(dmz*dwz,ks2),0.d0) )
      dzt=-dgwwz2/dcmplx( kt2-mz2 , dmax1(dsign(dmz*dwz,kt2),0.d0) )
      dw =-1.0d0/dcmplx( q2 -mw2 , dmax1(dsign(dmw*dww,q2 ),0.d0) )
c  for the running width, use below instead of the above dw.
c      dw =-1.0d0/dcmplx( q2 -mw2 , dmax1(dww*q2/dmw,0.d0) )
c
      w12=dw1(0)*dw2(0)-dw1(1)*dw2(1)-dw1(2)*dw2(2)-dw1(3)*dw2(3)
      w32=dw3(0)*dw2(0)-dw3(1)*dw2(1)-dw3(2)*dw2(2)-dw3(3)*dw2(3)
c
      p1w2= (p1(0)+ks(0))*dw2(0)-(p1(1)+ks(1))*dw2(1)
     &     -(p1(2)+ks(2))*dw2(2)-(p1(3)+ks(3))*dw2(3)
      p2w1= (p2(0)+ks(0))*dw1(0)-(p2(1)+ks(1))*dw1(1)
     &     -(p2(2)+ks(2))*dw1(2)-(p2(3)+ks(3))*dw1(3)
      p3w2= (p3(0)+kt(0))*dw2(0)-(p3(1)+kt(1))*dw2(1)
     &     -(p3(2)+kt(2))*dw2(2)-(p3(3)+kt(3))*dw2(3)
      p2w3= (p2(0)+kt(0))*dw3(0)-(p2(1)+kt(1))*dw3(1)
     &     -(p2(2)+kt(2))*dw3(2)-(p2(3)+kt(3))*dw3(3)
c
      jt12(0)= (p1(0)-p2(0))*w12 + p2w1*dw2(0) - p1w2*dw1(0)
      jt12(1)= (p1(1)-p2(1))*w12 + p2w1*dw2(1) - p1w2*dw1(1)
      jt12(2)= (p1(2)-p2(2))*w12 + p2w1*dw2(2) - p1w2*dw1(2)
      jt12(3)= (p1(3)-p2(3))*w12 + p2w1*dw2(3) - p1w2*dw1(3)
      jt32(0)= (p3(0)-p2(0))*w32 + p2w3*dw2(0) - p3w2*dw3(0)
      jt32(1)= (p3(1)-p2(1))*w32 + p2w3*dw2(1) - p3w2*dw3(1)
      jt32(2)= (p3(2)-p2(2))*w32 + p2w3*dw2(2) - p3w2*dw3(2)
      jt32(3)= (p3(3)-p2(3))*w32 + p2w3*dw2(3) - p3w2*dw3(3)
c
      jk12=(jt12(0)*ks(0)-jt12(1)*ks(1)-jt12(2)*ks(2)-jt12(3)*ks(3))/mz2
      jk32=(jt32(0)*kt(0)-jt32(1)*kt(1)-jt32(2)*kt(2)-jt32(3)*kt(3))/mz2
c
      j12(0)=jt12(0)*(das+dzs)-ks(0)*jk12*dzs
      j12(1)=jt12(1)*(das+dzs)-ks(1)*jk12*dzs
      j12(2)=jt12(2)*(das+dzs)-ks(2)*jk12*dzs
      j12(3)=jt12(3)*(das+dzs)-ks(3)*jk12*dzs
      j32(0)=jt32(0)*(dat+dzt)-kt(0)*jk32*dzt
      j32(1)=jt32(1)*(dat+dzt)-kt(1)*jk32*dzt
      j32(2)=jt32(2)*(dat+dzt)-kt(2)*jk32*dzt
      j32(3)=jt32(3)*(dat+dzt)-kt(3)*jk32*dzt
c
      jsw3=j12(0)*dw3(0)-j12(1)*dw3(1)-j12(2)*dw3(2)-j12(3)*dw3(3)
      jtw1=j32(0)*dw1(0)-j32(1)*dw1(1)-j32(2)*dw1(2)-j32(3)*dw1(3)
c
      p3js= (p3(0)-q(0))*j12(0)-(p3(1)-q(1))*j12(1)
     &     -(p3(2)-q(2))*j12(2)-(p3(3)-q(3))*j12(3)
      ksw3= (ks(0)-q(0))*dw3(0)-(ks(1)-q(1))*dw3(1)
     &     -(ks(2)-q(2))*dw3(2)-(ks(3)-q(3))*dw3(3)
      p1jt= (p1(0)-q(0))*j32(0)-(p1(1)-q(1))*j32(1)
     &     -(p1(2)-q(2))*j32(2)-(p1(3)-q(3))*j32(3)
      ktw1= (kt(0)-q(0))*dw1(0)-(kt(1)-q(1))*dw1(1)
     &     -(kt(2)-q(2))*dw1(2)-(kt(3)-q(3))*dw1(3)
c
      js(0)= (ks(0)-p3(0))*jsw3 + p3js*dw3(0) - ksw3*j12(0)
      js(1)= (ks(1)-p3(1))*jsw3 + p3js*dw3(1) - ksw3*j12(1)
      js(2)= (ks(2)-p3(2))*jsw3 + p3js*dw3(2) - ksw3*j12(2)
      js(3)= (ks(3)-p3(3))*jsw3 + p3js*dw3(3) - ksw3*j12(3)
      jt(0)= (kt(0)-p1(0))*jtw1 + p1jt*dw1(0) - ktw1*j32(0)
      jt(1)= (kt(1)-p1(1))*jtw1 + p1jt*dw1(1) - ktw1*j32(1)
      jt(2)= (kt(2)-p1(2))*jtw1 + p1jt*dw1(2) - ktw1*j32(2)
      jt(3)= (kt(3)-p1(3))*jtw1 + p1jt*dw1(3) - ktw1*j32(3)
c
      w13=dw1(0)*dw3(0)-dw1(1)*dw3(1)-dw1(2)*dw3(2)-dw1(3)*dw3(3)
c
      j4(0)=dgw2*( dw1(0)*w32 + dw3(0)*w12 - 2.d0*dw2(0)*w13 )
      j4(1)=dgw2*( dw1(1)*w32 + dw3(1)*w12 - 2.d0*dw2(1)*w13 )
      j4(2)=dgw2*( dw1(2)*w32 + dw3(2)*w12 - 2.d0*dw2(2)*w13 )
      j4(3)=dgw2*( dw1(3)*w32 + dw3(3)*w12 - 2.d0*dw2(3)*w13 )
c
c      jj(0)=js(0)+jt(0)+j4(0)
c      jj(1)=js(1)+jt(1)+j4(1)
c      jj(2)=js(2)+jt(2)+j4(2)
c      jj(3)=js(3)+jt(3)+j4(3)

      jj(0)=j4(0)
      jj(1)=j4(1)
      jj(2)=j4(2)
      jj(3)=j4(3)
c
      jq=(jj(0)*q(0)-jj(1)*q(1)-jj(2)*q(2)-jj(3)*q(3))/mw2
c

      jwww(1) = dcmplx( (jj(0)-jq*q(0))*dw )
      jwww(2) = dcmplx( (jj(1)-jq*q(1))*dw )
      jwww(3) = dcmplx( (jj(2)-jq*q(2))*dw )
      jwww(4) = dcmplx( (jj(3)-jq*q(3))*dw )
c
      return
      end subroutine

C
C ----------------------------------------------------------------------
C
      SUBROUTINE MOM2CX(ESUM,MASS1,MASS2,COSTH1,PHI1 , P1,P2)
C
C This subroutine sets up two four-momenta in the two particle rest     
C frame.                                                                
C                                                                       
C INPUT:                                                                
C       real    ESUM           : energy sum of particle 1 and 2         
C       real    MASS1          : mass            of particle 1          
C       real    MASS2          : mass            of particle 2          
C       real    COSTH1         : cos(theta)      of particle 1          
C       real    PHI1           : azimuthal angle of particle 1          
C                                                                       
C OUTPUT:                                                               
C       real    P1(0:3)        : four-momentum of particle 1            
C       real    P2(0:3)        : four-momentum of particle 2            
C
      REAL*8    P1(0:3),P2(0:3),
     &        ESUM,MASS1,MASS2,COSTH1,PHI1,MD2,ED,PP,SINTH1
C
      MD2=(MASS1-MASS2)*(MASS1+MASS2)
      ED=MD2/ESUM
      IF (MASS1*MASS2.EQ.0.) THEN
      PP=(ESUM-ABS(ED))*0.5d0
C
      ELSE
      PP=SQRT((MD2/ESUM)**2-2.0d0*(MASS1**2+MASS2**2)+ESUM**2)*0.5d0
      ENDIF
      SINTH1=SQRT((1.0d0-COSTH1)*(1.0d0+COSTH1))
C
      P1(0) = MAX((ESUM+ED)*0.5d0,0.d0)
      P1(1) = PP*SINTH1*COS(PHI1)
      P1(2) = PP*SINTH1*SIN(PHI1)
      P1(3) = PP*COSTH1
C
      P2(0) = MAX((ESUM-ED)*0.5d0,0.d0)
      P2(1) = -P1(1)
      P2(2) = -P1(2)
      P2(3) = -P1(3)
C
      RETURN
      end subroutine
C **********************************************************************
C
      SUBROUTINE MOMNTX(ENERGY,MASS,COSTH,PHI , P)
C
C This subroutine sets up a four-momentum from the four inputs.         
C                                                                       
C INPUT:                                                                
C       real    ENERGY         : energy                                 
C       real    MASS           : mass                                   
C       real    COSTH          : cos(theta)                             
C       real    PHI            : azimuthal angle                        
C                                                                       
C OUTPUT:                                                               
C       real    P(0:3)         : four-momentum                          
C
      implicit none
      REAL*8    P(0:3),ENERGY,MASS,COSTH,PHI,PP,SINTH
C
      P(0) = ENERGY
      IF (ENERGY.EQ.MASS) THEN
         P(1) = 0.
         P(2) = 0.
         P(3) = 0.
      ELSE
         PP=SQRT((ENERGY-MASS)*(ENERGY+MASS))
         SINTH=SQRT((1.-COSTH)*(1.+COSTH))
         P(3) = PP*COSTH
         IF (PHI.EQ.0.) THEN
            P(1) = PP*SINTH
            P(2) = 0.
         ELSE
            P(1) = PP*SINTH*COS(PHI)
            P(2) = PP*SINTH*SIN(PHI)
         ENDIF
      ENDIF
      RETURN
      end subroutine
C
c
c
c	Subroutine returns the desired fermion or
c	anti-fermion anti-spinor. ie., <f|
c	A replacement for the HELAS routine OXXXXX
c
c	Adam Duff,  1992 August 31
c	<duff@phenom.physics.wisc.edu>
c
      subroutine oxxxxx(
     &              p,		!in: four vector momentum
     &              fmass,	!in: fermion mass
     &              nhel,	!in: anti-spinor helicity, -1 or 1
     &              nsf,	!in: -1=antifermion, 1=fermion
     &              fo		!out: fermion wavefunction
     &                 )
      implicit none
c
c declare input/output variables
c
      complex*16 fo(6)
      integer*4 nhel, nsf
      real*8 p(0:3), fmass
c
c declare local variables
c
      real*8 r_zero, r_one, r_two
      parameter( r_zero=0.0d0, r_one=1.0d0, r_two=2.0d0 )
      complex*16 c_zero
c
      real*8 plat, pabs, omegap, omegam, rs2pa, spaz
      c_zero=dcmplx( r_zero, r_zero )
c
c define kinematic parameters
c
      fo(5) = dcmplx( p(0), p(3) ) * nsf
      fo(6) = dcmplx( p(1), p(2) ) * nsf
      plat = sqrt( p(1)**2 + p(2)**2 )
      pabs = sqrt( p(1)**2 + p(2)**2 + p(3)**2 )
      omegap = sqrt( p(0) + pabs )
c
c do massive fermion case
c
      if ( fmass .ne. r_zero ) then
         omegam = fmass / omegap
         if ( nsf .eq. 1 ) then
            if ( nhel .eq. 1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fo(1) = dcmplx( omegap, r_zero )
                     fo(2) = c_zero
                     fo(3) = dcmplx( omegam, r_zero )
                     fo(4) = c_zero
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs + p(3) )
                     fo(1) = omegap * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fo(2) = omegap * rs2pa / spaz
     &                     * dcmplx( p(1), -p(2) )
                     fo(3) = omegam * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fo(4) = omegam * rs2pa / spaz
     &                     * dcmplx( p(1), -p(2) )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = dcmplx( omegap, r_zero )
                     fo(3) = c_zero
                     fo(4) = dcmplx( omegam, r_zero )
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs - p(3) )
                     fo(1) = omegap * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fo(2) = omegap * rs2pa * spaz / plat
     &                     * dcmplx( p(1), -p(2) )
                     fo(3) = omegam * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fo(4) = omegam * rs2pa * spaz / plat
     &                     * dcmplx( p(1), -p(2) )
                  end if
               end if
            else if ( nhel .eq. -1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = dcmplx( omegam, r_zero )
                     fo(3) = c_zero
                     fo(4) = dcmplx( omegap, r_zero )
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs + p(3) )
                     fo(1) = omegam * rs2pa / spaz
     &                     * dcmplx( -p(1), -p(2) )
                     fo(2) = omegam * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fo(3) = omegap * rs2pa / spaz
     &                     * dcmplx( -p(1), -p(2) )
                     fo(4) = omegap * rs2pa
     &                     * dcmplx( spaz, r_zero )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fo(1) = dcmplx( -omegam, r_zero )
                     fo(2) = c_zero
                     fo(3) = dcmplx( -omegap, r_zero )
                     fo(4) = c_zero
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs - p(3) )
                     fo(1) = omegam * rs2pa * spaz / plat
     &                     * dcmplx( -p(1), -p(2) )
                     fo(2) = omegam * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fo(3) = omegap * rs2pa * spaz / plat
     &                     * dcmplx( -p(1), -p(2) )
                     fo(4) = omegap * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                  end if
               end if
            else
               stop 'oxxxxx:  fermion helicity must be +1,-1'
            end if
         else if ( nsf .eq. -1 ) then
            if ( nhel .eq. 1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = dcmplx( omegam, r_zero )
                     fo(3) = c_zero
                     fo(4) = dcmplx( -omegap, r_zero )
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs + p(3) )
                     fo(1) = omegam * rs2pa / spaz
     &                     * dcmplx( -p(1), -p(2) )
                     fo(2) = omegam * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fo(3) = -omegap * rs2pa / spaz
     &                     * dcmplx( -p(1), -p(2) )
                     fo(4) = -omegap * rs2pa
     &                     * dcmplx( spaz, r_zero )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fo(1) = dcmplx( -omegam, r_zero )
                     fo(2) = c_zero
                     fo(3) = dcmplx( omegap, r_zero )
                     fo(4) = c_zero
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs - p(3) )
                     fo(1) = omegam * rs2pa * spaz / plat
     &                     * dcmplx( -p(1), -p(2) )
                     fo(2) = omegam * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fo(3) = -omegap * rs2pa * spaz / plat
     &                     * dcmplx( -p(1), -p(2) )
                     fo(4) = -omegap * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                  end if
               end if
            else if ( nhel .eq. -1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fo(1) = dcmplx( -omegap, r_zero )
                     fo(2) = c_zero
                     fo(3) = dcmplx( omegam, r_zero )
                     fo(4) = c_zero
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs + p(3) )
                     fo(1) = -omegap * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fo(2) = -omegap * rs2pa / spaz
     &                     * dcmplx( p(1), -p(2) )
                     fo(3) = omegam * rs2pa
     &                     * dcmplx( spaz, r_zero )
                     fo(4) = omegam * rs2pa / spaz
     &                     * dcmplx( p(1), -p(2) )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = dcmplx( -omegap, r_zero )
                     fo(3) = c_zero
                     fo(4) = dcmplx( omegam, r_zero )
                  else
                     rs2pa = r_one / sqrt( r_two * pabs )
                     spaz = sqrt( pabs - p(3) )
                     fo(1) = -omegap * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fo(2) = -omegap * rs2pa * spaz / plat
     &                     * dcmplx( p(1), -p(2) )
                     fo(3) = omegam * rs2pa / spaz
     &                     * dcmplx( plat, r_zero )
                     fo(4) = omegam * rs2pa * spaz / plat
     &                     * dcmplx( p(1), -p(2) )
                  end if
               end if
            else
               stop 'oxxxxx:  fermion helicity must be +1,-1'
            end if
         else
            stop 'oxxxxx:  fermion type must be +1,-1'
         end if
c
c do massless case
c
      else
         if ( nsf .eq. 1 ) then
            if ( nhel .eq. 1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fo(1) = dcmplx( omegap, r_zero )
                     fo(2) = c_zero
                     fo(3) = c_zero
                     fo(4) = c_zero
                  else
                     spaz = sqrt( pabs + p(3) )
                     fo(1) = dcmplx( spaz, r_zero )
                     fo(2) = r_one / spaz
     &                     * dcmplx( p(1), -p(2) )
                     fo(3) = c_zero
                     fo(4) = c_zero
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = dcmplx( omegap, r_zero )
                     fo(3) = c_zero
                     fo(4) = c_zero
                  else
                     spaz = sqrt( pabs - p(3) )
                     fo(1) = r_one / spaz
     &                     * dcmplx( plat, r_zero )
                     fo(2) = spaz / plat
     &                     * dcmplx( p(1), -p(2) )
                     fo(3) = c_zero
                     fo(4) = c_zero
                  end if
               end if
            else if ( nhel .eq. -1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = c_zero
                     fo(3) = c_zero
                     fo(4) = dcmplx( omegap, r_zero )
                  else
                     spaz = sqrt( pabs + p(3) )
                     fo(1) = c_zero
                     fo(2) = c_zero
                     fo(3) = r_one / spaz
     &                     * dcmplx( -p(1), -p(2) )
                     fo(4) = dcmplx( spaz, r_zero )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = c_zero
                     fo(3) = dcmplx( -omegap, r_zero )
                     fo(4) = c_zero
                  else
                     spaz = sqrt( pabs - p(3) )
                     fo(1) = c_zero
                     fo(2) = c_zero
                     fo(3) = spaz / plat
     &                     * dcmplx( -p(1), -p(2) )
                     fo(4) = r_one / spaz
     &                     * dcmplx( plat, r_zero )
                  end if
               end if
            else
               stop 'oxxxxx:  fermion helicity must be +1,-1'
            end if
         else if ( nsf .eq. -1 ) then
            if ( nhel .eq. 1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = c_zero
                     fo(3) = c_zero
                     fo(4) = dcmplx( -omegap, r_zero )
                  else
                     spaz = sqrt( pabs + p(3) )
                     fo(1) = c_zero
                     fo(2) = c_zero
                     fo(3) = -r_one / spaz
     &                     * dcmplx( -p(1), -p(2) )
                     fo(4) = dcmplx( -spaz, r_zero )
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = c_zero
                     fo(3) = dcmplx( omegap, r_zero )
                     fo(4) = c_zero
                  else
                     spaz = sqrt( pabs - p(3) )
                     fo(1) = c_zero
                     fo(2) = c_zero
                     fo(3) = -spaz / plat
     &                     * dcmplx( -p(1), -p(2) )
                     fo(4) = -r_one / spaz
     &                     * dcmplx( plat, r_zero )
                  end if
               end if
            else if ( nhel .eq. -1 ) then
               if ( p(3) .ge. r_zero ) then
                  if ( plat .eq. r_zero ) then
                     fo(1) = dcmplx( -omegap, r_zero )
                     fo(2) = c_zero
                     fo(3) = c_zero
                     fo(4) = c_zero
                  else
                     spaz = sqrt( pabs + p(3) )
                     fo(1) = dcmplx( -spaz, r_zero )
                     fo(2) = -r_one / spaz
     &                     * dcmplx( p(1), -p(2) )
                     fo(3) = c_zero
                     fo(4) = c_zero
                  end if
               else
                  if ( plat .eq. r_zero ) then
                     fo(1) = c_zero
                     fo(2) = dcmplx( -omegap, r_zero )
                     fo(3) = c_zero
                     fo(4) = c_zero
                  else
                     spaz = sqrt( pabs - p(3) )
                     fo(1) = -r_one / spaz
     &                     * dcmplx( plat, r_zero )
                     fo(2) = -spaz / plat
     &                     * dcmplx( p(1), -p(2) )
                     fo(3) = c_zero
                     fo(4) = c_zero
                  end if
               end if
            else
               stop 'oxxxxx:  fermion helicity must be +1,-1'
            end if
         else
            stop 'oxxxxx:  fermion type must be +1,-1'
         end if
      end if
c
c done
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine rotxxx(p,q , prot)
c
c this subroutine performs the spacial rotation of a four-momentum.     
c the momentum p is assumed to be given in the frame where the spacial  
c component of q points the positive z-axis.  prot is the momentum p    
c rotated to the frame where q is given.                                
c                                                                       
c input:                                                                
c       real    p(0:3)         : four-momentum p in q(1)=q(2)=0 frame   
c       real    q(0:3)         : four-momentum q in the rotated frame   
c                                                                       
c output:                                                               
c       real    prot(0:3)      : four-momentum p in the rotated frame   
c
      real*8    p(0:3),q(0:3),prot(0:3),qt2,qt,psgn,qq,p1
c
      real*8 r_zero, r_one
      parameter( r_zero=0.0d0, r_one=1.0d0 )
c
      prot(0) = p(0)
c
      qt2=q(1)**2+q(2)**2
c
      if ( qt2 .eq. r_zero ) then
          if ( q(3) .eq. r_zero ) then
             prot(1) = p(1)
             prot(2) = p(2)
             prot(3) = p(3)
          else
             psgn=dsign(r_one,q(3))
             prot(1) = p(1)*psgn
             prot(2) = p(2)*psgn
             prot(3) = p(3)*psgn
          endif
      else
          qq=sqrt(qt2+q(3)**2)
          qt=sqrt(qt2)
          p1=p(1)
          prot(1) = q(1)*q(3)/qq/qt*p1 -q(2)/qt*p(2) +q(1)/qq*p(3)
          prot(2) = q(2)*q(3)/qq/qt*p1 +q(1)/qt*p(2) +q(2)/qq*p(3)
          prot(3) =          -qt/qq*p1               +q(3)/qq*p(3)
      endif
c
      return
      end subroutine
C ======================================================================
C
      SUBROUTINE SSSSXX(S1,S2,S3,S4,G , VERTEX)
C
C This subroutine computes an amplitude of the four-scalar coupling.    
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex S3(3)          : third  scalar                        S3
C       complex S4(3)          : fourth scalar                        S4
C       real    G              : coupling constant                 GHHHH
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude            Gamma(S1,S2,S3,S4)
C
      implicit none
      COMPLEX*16 S1(3),S2(3),S3(3),S4(3),VERTEX
      REAL*8     G
C
      VERTEX = G*S1(1)*S2(1)*S3(1)*S4(1)
C
      RETURN
      end subroutine
C
C ======================================================================
C
      SUBROUTINE SSSXXX(S1,S2,S3,G , VERTEX)
C
C This subroutine computes an amplitude of the three-scalar coupling.   
C                                                                       
C INPUT:                                                                
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex S3(3)          : third  scalar                        S3
C       real    G              : coupling constant                  GHHH
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude               Gamma(S1,S2,S3)
C
      implicit none
      COMPLEX*16 S1(3),S2(3),S3(3),VERTEX
      REAL*8    G
C
      VERTEX = G*S1(1)*S2(1)*S3(1)
C
      RETURN
      end subroutine
C
C
C ----------------------------------------------------------------------
C
      SUBROUTINE SXXXXX(P,NSS , SC)
C
C This subroutine computes a complex SCALAR wavefunction.               
C                                                                       
C INPUT:                                                                
C       real    P(0:3)         : four-momentum of scalar boson          
C       integer NSS  = -1 or 1 : +1 for final, -1 for initial           
C                                                                       
C OUTPUT:                                                               
C       complex SC(3)          : scalar wavefunction                   S
C
      COMPLEX*16 SC(3)
      REAL*8    P(0:3)
      INTEGER NSS
C
      SC(1) = DCMPLX( 1.0 )
      SC(2) = DCMPLX(P(0),P(3))*NSS
      SC(3) = DCMPLX(P(1),P(2))*NSS
C
      RETURN
      end subroutine
c
c ======================================================================
c
      subroutine vssxxx(vc,s1,s2,g , vertex)
c
c this subroutine computes an amplitude from the vector-scalar-scalar   
c coupling.  the coupling is absent in the minimal sm in unitary gauge. 
c                                                                       
c       complex vc(6)          : input  vector                        v 
c       complex s1(3)          : first  scalar                        s1
c       complex s2(3)          : second scalar                        s2
c       complex g              : coupling constant (s1 charge)          
c                                                                       
c examples of the coupling constant g for susy particles are as follows:
c   -----------------------------------------------------------         
c   |    s1    | (q,i3) of s1  ||   v=a   |   v=z   |   v=w   |         
c   -----------------------------------------------------------         
c   | nu~_l    | (  0  , +1/2) ||   ---   |  gzn(1) |  gwf(1) |         
c   | e~_l     | ( -1  , -1/2) ||  gal(1) |  gzl(1) |  gwf(1) |         
c   | u~_l     | (+2/3 , +1/2) ||  gau(1) |  gzu(1) |  gwf(1) |         
c   | d~_l     | (-1/3 , -1/2) ||  gad(1) |  gzd(1) |  gwf(1) |         
c   -----------------------------------------------------------         
c   | e~_r-bar | ( +1  ,  0  ) || -gal(2) | -gzl(2) | -gwf(2) |         
c   | u~_r-bar | (-2/3 ,  0  ) || -gau(2) | -gzu(2) | -gwf(2) |         
c   | d~_r-bar | (+1/3 ,  0  ) || -gad(2) | -gzd(2) | -gwf(2) |         
c   -----------------------------------------------------------         
c where the s1 charge is defined by the flowing-out quantum number.     
c                                                                       
c output:                                                               
c       complex vertex         : amplitude                gamma(v,s1,s2)
c
      complex*16 vc(6),s1(3),s2(3),vertex,g
      real*8    p(0:3)
c
      p(0)=dble( s1(2)-s2(2))
      p(1)=dble( s1(3)-s2(3))
      p(2)=dimag(s1(3)-s2(3))
      p(3)=dimag(s1(2)-s2(2))
c
      vertex = g*s1(1)*s2(1)
     &        *(vc(1)*p(0)-vc(2)*p(1)-vc(3)*p(2)-vc(4)*p(3))
c
      return
      end subroutine
C
      SUBROUTINE VVSSXX(V1,V2,S1,S2,G , VERTEX)
C
C This subroutine computes an amplitude of the vector-vector-scalar-    
C scalar coupling.                                                      
C                                                                       
C INPUT:                                                                
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant                 GVVHH
C                                                                       
C OUTPUT:                                                               
C       complex VERTEX         : amplitude            Gamma(V1,V2,S1,S2)
C
      implicit none
      COMPLEX*16 V1(6),V2(6),S1(3),S2(3),VERTEX
      REAL*8    G
C
      VERTEX = G*S1(1)*S2(1)
     &        *(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      end subroutine
C
c
c ======================================================================
c
      subroutine vvsxxx(v1,v2,sc,g , vertex)
c
c this subroutine computes an amplitude of the vector-vector-scalar     
c coupling.                                                             
c                                                                       
c input:                                                                
c       complex v1(6)          : first  vector                        v1
c       complex v2(6)          : second vector                        v2
c       complex sc(3)          : input  scalar                        s 
c       real    g              : coupling constant                  gvvh
c                                                                       
c output:                                                               
c       complex vertex         : amplitude                gamma(v1,v2,s)
c
      complex*16 v1(6),v2(6),sc(3),vertex
      real*8    g
c
      vertex = g*sc(1)*(v1(1)*v2(1)-v1(2)*v2(2)-v1(3)*v2(3)-v1(4)*v2(4))
c
      return
      end subroutine
c
c ======================================================================
c
      subroutine vvvxxx(wm,wp,w3,g , vertex)
c
c this subroutine computes an amplitude of the three-point coupling of  
c the gauge bosons.                                                     
c                                                                       
c input:                                                                
c       complex wm(6)          : vector               flow-out w-       
c       complex wp(6)          : vector               flow-out w+       
c       complex w3(6)          : vector               j3 or a    or z   
c       real    g              : coupling constant    gw or gwwa or gwwz
c                                                                       
c output:                                                               
c       complex vertex         : amplitude               gamma(wm,wp,w3)
c
      complex*16 wm(6),wp(6),w3(6),vertex, 
     &        xv1,xv2,xv3,v12,v23,v31,p12,p13,p21,p23,p31,p32
      real*8    pwm(0:3),pwp(0:3),pw3(0:3),g
c
      real*8 r_zero, r_tenth
      parameter( r_zero=0.0d0, r_tenth=0.1d0 )
c
      pwm(0)=dble( wm(5))
      pwm(1)=dble( wm(6))
      pwm(2)=dimag(wm(6))
      pwm(3)=dimag(wm(5))
      pwp(0)=dble( wp(5))
      pwp(1)=dble( wp(6))
      pwp(2)=dimag(wp(6))
      pwp(3)=dimag(wp(5))
      pw3(0)=dble( w3(5))
      pw3(1)=dble( w3(6))
      pw3(2)=dimag(w3(6))
      pw3(3)=dimag(w3(5))
c
      v12=wm(1)*wp(1)-wm(2)*wp(2)-wm(3)*wp(3)-wm(4)*wp(4)
      v23=wp(1)*w3(1)-wp(2)*w3(2)-wp(3)*w3(3)-wp(4)*w3(4)
      v31=w3(1)*wm(1)-w3(2)*wm(2)-w3(3)*wm(3)-w3(4)*wm(4)
      xv1=r_zero
      xv2=r_zero
      xv3=r_zero
      if ( abs(wm(1)) .ne. r_zero ) then
         if (abs(wm(1)).ge.max(abs(wm(2)),abs(wm(3)),abs(wm(4)))
     $        *r_tenth)
     &      xv1=pwm(0)/wm(1)
      endif
      if ( abs(wp(1)) .ne. r_zero) then
         if (abs(wp(1)).ge.max(abs(wp(2)),abs(wp(3)),abs(wp(4)))
     $        *r_tenth)
     &      xv2=pwp(0)/wp(1)
      endif
      if ( abs(w3(1)) .ne. r_zero) then
         if ( abs(w3(1)).ge.max(abs(w3(2)),abs(w3(3)),abs(w3(4)))
     $        *r_tenth)
     &      xv3=pw3(0)/w3(1)
      endif
      p12= (pwm(0)-xv1*wm(1))*wp(1)-(pwm(1)-xv1*wm(2))*wp(2)
     &    -(pwm(2)-xv1*wm(3))*wp(3)-(pwm(3)-xv1*wm(4))*wp(4)
      p13= (pwm(0)-xv1*wm(1))*w3(1)-(pwm(1)-xv1*wm(2))*w3(2)
     &    -(pwm(2)-xv1*wm(3))*w3(3)-(pwm(3)-xv1*wm(4))*w3(4)
      p21= (pwp(0)-xv2*wp(1))*wm(1)-(pwp(1)-xv2*wp(2))*wm(2)
     &    -(pwp(2)-xv2*wp(3))*wm(3)-(pwp(3)-xv2*wp(4))*wm(4)
      p23= (pwp(0)-xv2*wp(1))*w3(1)-(pwp(1)-xv2*wp(2))*w3(2)
     &    -(pwp(2)-xv2*wp(3))*w3(3)-(pwp(3)-xv2*wp(4))*w3(4)
      p31= (pw3(0)-xv3*w3(1))*wm(1)-(pw3(1)-xv3*w3(2))*wm(2)
     &    -(pw3(2)-xv3*w3(3))*wm(3)-(pw3(3)-xv3*w3(4))*wm(4)
      p32= (pw3(0)-xv3*w3(1))*wp(1)-(pw3(1)-xv3*w3(2))*wp(2)
     &    -(pw3(2)-xv3*w3(3))*wp(3)-(pw3(3)-xv3*w3(4))*wp(4)
c
      vertex = -(v12*(p13-p23)+v23*(p21-p31)+v31*(p32-p12))*g
c
      return
      end subroutine
c
c
c	Subroutine returns the value of evaluated
c	helicity basis boson polarisation wavefunction.
c	Replaces the HELAS routine VXXXXX
c
c	Adam Duff,  1992 September 3
c	<duff@phenom.physics.wisc.edu>
c
      subroutine vxxxxx(
     &              p,		!in: boson four momentum
     &              vmass,	!in: boson mass
     &              nhel,	!in: boson helicity
     &              nsv,	!in: incoming (-1) or outgoing (+1)
     &              vc		!out: boson wavefunction
     &                 )
      implicit none
c
c declare input/output variables
c
      complex*16 vc(6)
      integer*4 nhel, nsv
      real*8 p(0:3), vmass
c
c declare local variables
c
      real*8 r_zero, r_one, r_two
      parameter( r_zero=0.0d0, r_one=1.0d0, r_two=2.0d0 )
      complex*16 c_zero
c
      real*8 plat, pabs, rs2, rplat, rpabs, rden
      c_zero=dcmplx( r_zero, r_zero )
c
c define internal/external momenta
c
      if ( nsv**2 .ne. 1 ) then
         stop 'vxxxxx:  nsv is not one of -1, +1'
      end if
c
      rs2 = sqrt( r_one / r_two )
      vc(5) = dcmplx( p(0), p(3) ) * nsv
      vc(6) = dcmplx( p(1), p(2) ) * nsv
      plat = sqrt( p(1)**2 + p(2)**2 )
      pabs = sqrt( p(1)**2 + p(2)**2 + p(3)**2 )
c
c calculate polarisation four vectors
c
      if ( nhel**2 .eq. 1 ) then
         if ( (pabs .eq. r_zero) .or. (plat .eq. r_zero) ) then
            vc(1) = c_zero
            vc(2) = dcmplx( -nhel * rs2 * dsign( r_one, p(3) ), r_zero )
            vc(3) = dcmplx( r_zero, nsv * rs2 )
            vc(4) = c_zero
         else
            rplat = r_one / plat
            rpabs = r_one / pabs
            vc(1) = c_zero
            vc(2) = dcmplx( -nhel * rs2 * rpabs * rplat * p(1) * p(3),
     &                     -nsv * rs2 * rplat * p(2) )
            vc(3) = dcmplx( -nhel * rs2 * rpabs * rplat * p(2) * p(3),
     &                     nsv * rs2 * rplat * p(1) )
            vc(4) = dcmplx( nhel * rs2 * rpabs * plat,
     &                     r_zero )
         end if
      else if ( nhel .eq. 0 ) then
         if ( vmass .gt. r_zero ) then
            if ( pabs .eq. r_zero ) then
               vc(1) = c_zero
               vc(2) = c_zero
               vc(3) = c_zero
               vc(4) = dcmplx( r_one, r_zero )
            else
               rden = p(0) / ( vmass * pabs )
               vc(1) = dcmplx( pabs / vmass, r_zero )
               vc(2) = dcmplx( rden * p(1), r_zero )
               vc(3) = dcmplx( rden * p(2), r_zero )
               vc(4) = dcmplx( rden * p(3), r_zero )
            end if
         else
            stop  'vxxxxx: nhel = 0 is only for massive bosons'
         end if
      else if ( nhel .eq. 4 ) then
         if ( vmass .gt. r_zero ) then
            rden = r_one / vmass
            vc(1) = dcmplx( rden * p(0), r_zero )
            vc(2) = dcmplx( rden * p(1), r_zero )
            vc(3) = dcmplx( rden * p(2), r_zero )
            vc(4) = dcmplx( rden * p(3), r_zero )
         elseif (vmass .eq. r_zero) then
            rden = r_one / p(0)
            vc(1) = dcmplx( rden * p(0), r_zero )
            vc(2) = dcmplx( rden * p(1), r_zero )
            vc(3) = dcmplx( rden * p(2), r_zero )
            vc(4) = dcmplx( rden * p(3), r_zero )
         else
            stop 'vxxxxx: nhel = 4 is only for m>=0'
         end if
      else
         stop 'vxxxxx:  nhel is not one of -1, 0, 1 or 4'
      end if
c
c done
c
      return
      end subroutine
c
c ----------------------------------------------------------------------
c
      subroutine w3w3xx(wm,w31,wp,w32,g31,g32,wmass,wwidth , vertex)
c
c this subroutine computes an amplitude of the four-point coupling of   
c the w-, w+ and two w3/z/a.  the amplitude includes the contributions  
c of w exchange diagrams.  the internal w propagator is given in unitary
c gauge.  if one sets wmass=0.0, then the gggg vertex is given (see sect
c 2.9.1 of the manual).
c                                                                       
c input:                                                                
c       complex wm(0:3)        : flow-out w-                         wm 
c       complex w31(0:3)       : first    w3/z/a                     w31
c       complex wp(0:3)        : flow-out w+                         wp 
c       complex w32(0:3)       : second   w3/z/a                     w32
c       real    g31            : coupling of w31 with w-/w+             
c       real    g32            : coupling of w32 with w-/w+             
c                                                  (see the table below)
c       real    wmass          : mass  of w                             
c       real    wwidth         : width of w                             
c                                                                       
c the possible sets of the inputs are as follows:                       
c   -------------------------------------------                         
c   |  wm  |  w31 |  wp  |  w32 |  g31 |  g32 |                         
c   -------------------------------------------                         
c   |  w-  |  w3  |  w+  |  w3  |  gw  |  gw  |                         
c   |  w-  |  w3  |  w+  |  z   |  gw  | gwwz |                         
c   |  w-  |  w3  |  w+  |  a   |  gw  | gwwa |                         
c   |  w-  |  z   |  w+  |  z   | gwwz | gwwz |                         
c   |  w-  |  z   |  w+  |  a   | gwwz | gwwa |                         
c   |  w-  |  a   |  w+  |  a   | gwwa | gwwa |                         
c   -------------------------------------------                         
c where all the bosons are defined by the flowing-out quantum number.   
c                                                                       
c output:                                                               
c       complex vertex         : amplitude          gamma(wm,w31,wp,w32)
c
      complex*16    wm(6),w31(6),wp(6),w32(6),vertex
      complex*16 dv1(0:3),dv2(0:3),dv3(0:3),dv4(0:3),dvertx,
     &           v12,v13,v14,v23,v24,v34
      real*8     g31,g32,wmass,wwidth
c
      real*8 r_zero, r_one
      parameter( r_zero=0.0d0, r_one=1.0d0 )

      dv1(0)=dcmplx(wm(1))
      dv1(1)=dcmplx(wm(2))
      dv1(2)=dcmplx(wm(3))
      dv1(3)=dcmplx(wm(4))
      dv2(0)=dcmplx(w31(1))
      dv2(1)=dcmplx(w31(2))
      dv2(2)=dcmplx(w31(3))
      dv2(3)=dcmplx(w31(4))
      dv3(0)=dcmplx(wp(1))
      dv3(1)=dcmplx(wp(2))
      dv3(2)=dcmplx(wp(3))
      dv3(3)=dcmplx(wp(4))
      dv4(0)=dcmplx(w32(1))
      dv4(1)=dcmplx(w32(2))
      dv4(2)=dcmplx(w32(3))
      dv4(3)=dcmplx(w32(4))
c
      if ( dble(wmass) .ne. r_zero ) then
c         dm2inv = r_one / dmw2
c
         v12= dv1(0)*dv2(0)-dv1(1)*dv2(1)-dv1(2)*dv2(2)-dv1(3)*dv2(3)
         v13= dv1(0)*dv3(0)-dv1(1)*dv3(1)-dv1(2)*dv3(2)-dv1(3)*dv3(3)
         v14= dv1(0)*dv4(0)-dv1(1)*dv4(1)-dv1(2)*dv4(2)-dv1(3)*dv4(3)
         v23= dv2(0)*dv3(0)-dv2(1)*dv3(1)-dv2(2)*dv3(2)-dv2(3)*dv3(3)
         v24= dv2(0)*dv4(0)-dv2(1)*dv4(1)-dv2(2)*dv4(2)-dv2(3)*dv4(3)
         v34= dv3(0)*dv4(0)-dv3(1)*dv4(1)-dv3(2)*dv4(2)-dv3(3)*dv4(3)
c
         dvertx = v12*v34 +v14*v23 -2.d0*v13*v24
c
         vertex = dcmplx( dvertx ) * (g31*g32)
c
      else
         v12= dv1(0)*dv2(0)-dv1(1)*dv2(1)-dv1(2)*dv2(2)-dv1(3)*dv2(3)
         v13= dv1(0)*dv3(0)-dv1(1)*dv3(1)-dv1(2)*dv3(2)-dv1(3)*dv3(3)
         v14= dv1(0)*dv4(0)-dv1(1)*dv4(1)-dv1(2)*dv4(2)-dv1(3)*dv4(3)
         v23= dv2(0)*dv3(0)-dv2(1)*dv3(1)-dv2(2)*dv3(2)-dv2(3)*dv3(3)
         v24= dv2(0)*dv4(0)-dv2(1)*dv4(1)-dv2(2)*dv4(2)-dv2(3)*dv4(3)
         v34= dv3(0)*dv4(0)-dv3(1)*dv4(1)-dv3(2)*dv4(2)-dv3(3)*dv4(3)
c

         dvertx = v14*v23 -v13*v24
c
         vertex = dcmplx( dvertx ) * (g31*g32)
      end if
c
      return
      end subroutine
c
c ======================================================================
c
      subroutine wwwwxx(wm1,wp1,wm2,wp2,gwwa,gwwz,zmass,zwidth , vertex)
c
c this subroutine computes an amplitude of the four-point w-/w+         
c coupling, including the contributions of photon and z exchanges.  the 
c photon propagator is given in feynman gauge and the z propagator is   
c given in unitary gauge.                                               
c                                                                       
c input:                                                                
c       complex wm1(0:3)       : first  flow-out w-                  wm1
c       complex wp1(0:3)       : first  flow-out w+                  wp1
c       complex wm2(0:3)       : second flow-out w-                  wm2
c       complex wp2(0:3)       : second flow-out w+                  wp2
c       real    gwwa           : coupling constant of w and a       gwwa
c       real    gwwz           : coupling constant of w and z       gwwz
c       real    zmass          : mass  of z                             
c       real    zwidth         : width of z                             
c                                                                       
c output:                                                               
c       complex vertex         : amplitude        gamma(wm1,wp1,wm2,wp2)
c
      complex*16    wm1(6),wp1(6),wm2(6),wp2(6),vertex
      complex*16 dv1(0:3),dv2(0:3),dv3(0:3),dv4(0:3),
     &           j12(0:3),j34(0:3),j14(0:3),j32(0:3),dvertx,
     &           sv1,sv2,sv3,sv4,tv1,tv2,tv3,tv4,dzs,dzt,
     &           v12,v13,v14,v23,v24,v34,js12,js34,js14,js32,js,jt
      real*8       pwm1(0:3),pwp1(0:3),pwm2(0:3),pwp2(0:3),
     &           gwwa,gwwz,zmass,zwidth
      real*8     q(0:3),k(0:3),dp1(0:3),dp2(0:3),dp3(0:3),dp4(0:3),
     &           dgwwa2,dgwwz2,dgw2,dmz,dwidth,s,t,das,dat
c
      real*8 r_zero, r_one, r_two
      parameter( r_zero=0.0d0, r_one=1.0d0, r_two=2.0d0 )
c
      pwm1(0)=dble( wm1(5))
      pwm1(1)=dble( wm1(6))
      pwm1(2)=dimag(wm1(6))
      pwm1(3)=dimag(wm1(5))
      pwp1(0)=dble( wp1(5))
      pwp1(1)=dble( wp1(6))
      pwp1(2)=dimag(wp1(6))
      pwp1(3)=dimag(wp1(5))
      pwm2(0)=dble( wm2(5))
      pwm2(1)=dble( wm2(6))
      pwm2(2)=dimag(wm2(6))
      pwm2(3)=dimag(wm2(5))
      pwp2(0)=dble( wp2(5))
      pwp2(1)=dble( wp2(6))
      pwp2(2)=dimag(wp2(6))
      pwp2(3)=dimag(wp2(5))
c
      dv1(0)=dcmplx(wm1(1))
      dv1(1)=dcmplx(wm1(2))
      dv1(2)=dcmplx(wm1(3))
      dv1(3)=dcmplx(wm1(4))
      dp1(0)=dble(pwm1(0))
      dp1(1)=dble(pwm1(1))
      dp1(2)=dble(pwm1(2))
      dp1(3)=dble(pwm1(3))
      dv2(0)=dcmplx(wp1(1))
      dv2(1)=dcmplx(wp1(2))
      dv2(2)=dcmplx(wp1(3))
      dv2(3)=dcmplx(wp1(4))
      dp2(0)=dble(pwp1(0))
      dp2(1)=dble(pwp1(1))
      dp2(2)=dble(pwp1(2))
      dp2(3)=dble(pwp1(3))
      dv3(0)=dcmplx(wm2(1))
      dv3(1)=dcmplx(wm2(2))
      dv3(2)=dcmplx(wm2(3))
      dv3(3)=dcmplx(wm2(4))
      dp3(0)=dble(pwm2(0))
      dp3(1)=dble(pwm2(1))
      dp3(2)=dble(pwm2(2))
      dp3(3)=dble(pwm2(3))
      dv4(0)=dcmplx(wp2(1))
      dv4(1)=dcmplx(wp2(2))
      dv4(2)=dcmplx(wp2(3))
      dv4(3)=dcmplx(wp2(4))
      dp4(0)=dble(pwp2(0))
      dp4(1)=dble(pwp2(1))
      dp4(2)=dble(pwp2(2))
      dp4(3)=dble(pwp2(3))
      dgwwa2=dble(gwwa)**2
      dgwwz2=dble(gwwz)**2
      dgw2  =dgwwa2+dgwwz2
      dmz   =dble(zmass)
      dwidth=dble(zwidth)
c
      v12= dv1(0)*dv2(0)-dv1(1)*dv2(1)-dv1(2)*dv2(2)-dv1(3)*dv2(3)
      v13= dv1(0)*dv3(0)-dv1(1)*dv3(1)-dv1(2)*dv3(2)-dv1(3)*dv3(3)
      v14= dv1(0)*dv4(0)-dv1(1)*dv4(1)-dv1(2)*dv4(2)-dv1(3)*dv4(3)
      v23= dv2(0)*dv3(0)-dv2(1)*dv3(1)-dv2(2)*dv3(2)-dv2(3)*dv3(3)
      v24= dv2(0)*dv4(0)-dv2(1)*dv4(1)-dv2(2)*dv4(2)-dv2(3)*dv4(3)
      v34= dv3(0)*dv4(0)-dv3(1)*dv4(1)-dv3(2)*dv4(2)-dv3(3)*dv4(3)
c
      q(0)=dp1(0)+dp2(0)
      q(1)=dp1(1)+dp2(1)
      q(2)=dp1(2)+dp2(2)
      q(3)=dp1(3)+dp2(3)
      k(0)=dp1(0)+dp4(0)
      k(1)=dp1(1)+dp4(1)
      k(2)=dp1(2)+dp4(2)
      k(3)=dp1(3)+dp4(3)
c
      s=q(0)**2-q(1)**2-q(2)**2-q(3)**2
      t=k(0)**2-k(1)**2-k(2)**2-k(3)**2
c
      das=-r_one/s
      dat=-r_one/t
      dzs=-r_one/dcmplx( s-dmz**2 , dmax1(dsign(dmz*dwidth,s),r_zero) )
      dzt=-r_one/dcmplx( t-dmz**2 , dmax1(dsign(dmz*dwidth,t),r_zero) )
c
      sv1= (dp2(0)+q(0))*dv1(0) -(dp2(1)+q(1))*dv1(1)
     &    -(dp2(2)+q(2))*dv1(2) -(dp2(3)+q(3))*dv1(3)
      sv2=-(dp1(0)+q(0))*dv2(0) +(dp1(1)+q(1))*dv2(1)
     &    +(dp1(2)+q(2))*dv2(2) +(dp1(3)+q(3))*dv2(3)
      sv3= (dp4(0)-q(0))*dv3(0) -(dp4(1)-q(1))*dv3(1)
     &    -(dp4(2)-q(2))*dv3(2) -(dp4(3)-q(3))*dv3(3)
      sv4=-(dp3(0)-q(0))*dv4(0) +(dp3(1)-q(1))*dv4(1)
     &    +(dp3(2)-q(2))*dv4(2) +(dp3(3)-q(3))*dv4(3)
c
      tv1= (dp4(0)+k(0))*dv1(0) -(dp4(1)+k(1))*dv1(1)
     &    -(dp4(2)+k(2))*dv1(2) -(dp4(3)+k(3))*dv1(3)
      tv2=-(dp3(0)-k(0))*dv2(0) +(dp3(1)-k(1))*dv2(1)
     &    +(dp3(2)-k(2))*dv2(2) +(dp3(3)-k(3))*dv2(3)
      tv3= (dp2(0)-k(0))*dv3(0) -(dp2(1)-k(1))*dv3(1)
     &    -(dp2(2)-k(2))*dv3(2) -(dp2(3)-k(3))*dv3(3)
      tv4=-(dp1(0)+k(0))*dv4(0) +(dp1(1)+k(1))*dv4(1)
     &    +(dp1(2)+k(2))*dv4(2) +(dp1(3)+k(3))*dv4(3)
c
      j12(0)=(dp1(0)-dp2(0))*v12 +sv1*dv2(0) +sv2*dv1(0)
      j12(1)=(dp1(1)-dp2(1))*v12 +sv1*dv2(1) +sv2*dv1(1)
      j12(2)=(dp1(2)-dp2(2))*v12 +sv1*dv2(2) +sv2*dv1(2)
      j12(3)=(dp1(3)-dp2(3))*v12 +sv1*dv2(3) +sv2*dv1(3)
      j34(0)=(dp3(0)-dp4(0))*v34 +sv3*dv4(0) +sv4*dv3(0)
      j34(1)=(dp3(1)-dp4(1))*v34 +sv3*dv4(1) +sv4*dv3(1)
      j34(2)=(dp3(2)-dp4(2))*v34 +sv3*dv4(2) +sv4*dv3(2)
      j34(3)=(dp3(3)-dp4(3))*v34 +sv3*dv4(3) +sv4*dv3(3)
c
      j14(0)=(dp1(0)-dp4(0))*v14 +tv1*dv4(0) +tv4*dv1(0)
      j14(1)=(dp1(1)-dp4(1))*v14 +tv1*dv4(1) +tv4*dv1(1)
      j14(2)=(dp1(2)-dp4(2))*v14 +tv1*dv4(2) +tv4*dv1(2)
      j14(3)=(dp1(3)-dp4(3))*v14 +tv1*dv4(3) +tv4*dv1(3)
      j32(0)=(dp3(0)-dp2(0))*v23 +tv3*dv2(0) +tv2*dv3(0)
      j32(1)=(dp3(1)-dp2(1))*v23 +tv3*dv2(1) +tv2*dv3(1)
      j32(2)=(dp3(2)-dp2(2))*v23 +tv3*dv2(2) +tv2*dv3(2)
      j32(3)=(dp3(3)-dp2(3))*v23 +tv3*dv2(3) +tv2*dv3(3)
c
      js12=q(0)*j12(0)-q(1)*j12(1)-q(2)*j12(2)-q(3)*j12(3)
      js34=q(0)*j34(0)-q(1)*j34(1)-q(2)*j34(2)-q(3)*j34(3)
      js14=k(0)*j14(0)-k(1)*j14(1)-k(2)*j14(2)-k(3)*j14(3)
      js32=k(0)*j32(0)-k(1)*j32(1)-k(2)*j32(2)-k(3)*j32(3)
c
      js=j12(0)*j34(0)-j12(1)*j34(1)-j12(2)*j34(2)-j12(3)*j34(3)
      jt=j14(0)*j32(0)-j14(1)*j32(1)-j14(2)*j32(2)-j14(3)*j32(3)
c
      dvertx = (v12*v34 +v14*v23 -r_two*v13*v24)*dgw2

c     &        +(dzs*dgwwz2+das*dgwwa2)*js -dzs*dgwwz2*js12*js34/dmz**2
c     &        +(dzt*dgwwz2+dat*dgwwa2)*jt -dzt*dgwwz2*js14*js32/dmz**2
c
      vertex = -dcmplx( dvertx )
c
      return
      end subroutine
      end module dhelas95
