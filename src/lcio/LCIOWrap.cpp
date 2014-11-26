//////////////////////////////////////////////////////////////////////////
// Interface for building LCIO events
//////////////////////////////////////////////////////////////////////////

#include "lcio.h"
#include "IO/LCWriter.h"
#include "IMPL/LCEventImpl.h"
#include "IMPL/LCRunHeaderImpl.h"
#include "IMPL/MCParticleImpl.h"
#include "IMPL/LCCollectionVec.h"
#include "UTIL/LCTime.h"

using namespace std;
using namespace lcio;

// Tell the caller that this is the true LCIO library
extern "C" bool lcio_available() {
  return true;
}

//////////////////////////////////////////////////////////////////////////
// LCEventImpl functions

extern "C" LCEventImpl* new_lcevent ( int proc_id, int event_id ) {
  LCEventImpl* evt = new LCEventImpl();
  evt->setRunNumber ( proc_id );
  evt->setEventNumber ( event_id );
  LCTime now;
  evt->setTimeStamp ( now.timeStamp() );
  return evt;
}

extern "C" void lcevent_delete( LCEventImpl* evt) {
  delete evt;
}

extern "C" void lcio_set_weight( LCEventImpl* evt, double wgt ) {
  evt->setWeight ( wgt );
}

//////////////////////////////////////////////////////////////////////////
// MCParticle functions

extern "C" LCCollectionVec* new_lccollection() {
  return new LCCollectionVec;
}

extern "C" MCParticleImpl* new_lcio_particle 
(double mom[3], int pdg, double mass, int status) {
  MCParticleImpl* mcp = new MCParticleImpl() ;
  mcp->setPDG ( pdg );
  mcp->setMomentum ( mom );
  mcp->setMass ( mass );
  mcp->setGeneratorStatus ( status );
  mcp->setCreatedInSimulation (true);
  return mcp; 
}

extern "C" MCParticleImpl* lcio_set_color_flow
(MCParticleImpl* mcp, int cflow[2]) {
  mcp->setColorFlow ( cflow );
}

extern "C" const int* lcio_particle_get_flow
( MCParticleImpl* mcp) {  
  return mcp->getColorFlow();
}

//////////////////////////////////////////////////////////////////////////
// LCWriter functions

extern "C" LCWriter* lcWrt () {
  LCWriter* lcWrt = LCFactory::getInstance()->createLCWriter();
}

extern "C" LCWriter* open_lcio_file_new 
( LCWriter* lcWrt, char* filename ) {
  lcWrt->open( filename, LCIO::WRITE_NEW );
}

extern "C" LCWriter* open_lcio_file_append
( LCWriter* lcWrt, char* filename ) {
  lcWrt->open( filename, LCIO::WRITE_APPEND );
}

//////////////////////////////////////////////////////////////////////////
// LCRunHeader functions

extern "C" LCRunHeaderImpl* runHdr() {
  return new LCRunHeaderImpl;
}

extern "C" int runhdr_set_run_number( LCRunHeaderImpl* runHdr, int rn ) {
  runHdr->setRunNumber (rn);
}

extern "C" void write_run_header 
(LCWriter* lcWrt, const LCRunHeaderImpl* runHdr, int complevel) {
  lcWrt->writeRunHeader (runHdr);
  lcWrt->setCompressionLevel (complevel);
}

