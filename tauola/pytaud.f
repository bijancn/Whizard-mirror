      SUBROUTINE PYTAUD(ITAU,IORIG,KFORIG,NDECAY)
C*********************************************************************
 
C...PYTAUD
C...Dummy routine, to be replaced by user, to handle the decay of a
C...polarized tau lepton.
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

 
CDEBUG      print *,"###############################################"
CDEBUG      print *,"###### MY PYTAUD WAS CALLED ###################"
CDEBUG      print *," ITAU,IORIG,KFORIG,NDECAY=",ITAU,IORIG,KFORIG,NDECAY
CDEBUG      print *,"###############################################"

C      IS_INITIALIZED = 1
C      IF ( IS_INITIALIZED .EQ. 0 ) THEN 
C         CALL TAUOLA(-1, 1)
C         JAK1=MSTP(198)
C         JAK2=MSTP(199)
C         print *,' TAUOLA initialized with JAK1, JAK2=',JAK1,JAK2
C         IS_INITIALIZED = 1
C      END IF

      CALL HANDLE_PYTAUD(ITAU,IORIG,KFORIG,NDECAY)

      RETURN
      END
