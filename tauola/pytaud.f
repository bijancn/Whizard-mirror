      SUBROUTINE PYTAUD(ITAU,IORIG,KFORIG,NDECAY)
C*********************************************************************
C...PYTAUD
C...Routine to handle the decay of a polarized tau lepton.
C...Input:
C...ITAU is the position where the decaying tau is stored in /PYJETS/.
C...IORIG is the position where the mother of the tau is stored;
C...     is 0 when the mother is not stored.
C...KFORIG is the flavour of the mother of the tau;
C...     is 0 when the mother is not known.
C...Note that IORIG=0 does not necessarily imply KFORIG=0;
C...     e.g. in B hadron semileptonic decays the W  propagator
C...     is not explicitly stored but the W code is still unambiguous.
C...Output:
C...NDECAY is the number of decay products in the current tau decay.
C...These decay products should be added to the /PYJETS/ common block,
C...in positions N+1 through N+NDECAY. For each product I you must
C...give the flavour codes K(I,2) and the five-momenta P(I,1), P(I,2),
C...P(I,3), P(I,4) and P(I,5). The rest will be stored automatically.
      use ilc_tauola_mod2
      IMPLICIT NONE
      INTEGER ITAU,IORIG,KFORIG
      INTEGER NDECAY
      INTEGER MSTP(200), MSTI(200)
      REAL*8  PARP(200), PARI(200)
      COMMON /PYPARS/ MSTP, PARP, MSTI, PARI
      INTEGER JAK1, JAK2, JAKP, JAKM, KTOM
      COMMON /JAKI/ JAK1, JAK2, JAKP, JAKM, KTOM
      INTEGER IS_INITIALIZED
      SAVE IS_INITIALIZED
      DATA IS_INITIALIZED/0/
      print *,"###############################################"
      print *,"###### MY PYTAUD WAS CALLED ###################"
      print *," ITAU,IORIG,KFORIG=",ITAU,IORIG,KFORIG
      print *,"###############################################"

      call ilc_tauola_pytaud(itau,iorig,kforig,ndecay)

      print *, 'Number of decay products: NDECAY =', NDECAY !!! Debugging
      END
