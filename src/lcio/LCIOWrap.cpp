//////////////////////////////////////////////////////////////////////////
// Interface for building LCIO events
//////////////////////////////////////////////////////////////////////////
#include<string>

#include "lcio.h"
#include "IO/LCWriter.h"
#include "EVENT/LCIO.h"
#include "IMPL/LCEventImpl.h"
#include "IMPL/LCRunHeaderImpl.h"
#include "IMPL/LCCollectionVec.h"
#include "IMPL/MCParticleImpl.h"
#include "IMPL/LCTOOLS.h"
#include "UTIL/LCTime.h"

using namespace std;
using namespace lcio;
using namespace IMPL;

// Tell the caller that this is the true LCIO library
extern "C" bool lcio_available() {
  return true;
}

//////////////////////////////////////////////////////////////////////////
// LCEventImpl functions

extern "C" LCEventImpl* new_lcio_event ( int proc_id, int event_id ) {
  LCEventImpl* evt = new LCEventImpl();
  evt->setRunNumber ( proc_id );
  evt->setEventNumber ( event_id );
  LCTime now;
  evt->setTimeStamp ( now.timeStamp() );
  return evt;
}

extern "C" void lcio_event_delete( LCEventImpl* evt) {
  delete evt;
}

extern "C" void lcio_set_weight( LCEventImpl* evt, double wgt ) {
  evt->setWeight ( wgt );
}

// dump the event to the screen

extern "C" void dump_lcio_event ( LCEventImpl* evt) {
  LCTOOLS::dumpEvent ( evt );
}

// add collection to LCIO event

extern "C" void lcio_event_add_collection
( LCEventImpl* evt, LCCollectionVec* mcVec ) {
  evt->addCollection( mcVec, "MCParticle" );
}

//////////////////////////////////////////////////////////////////////////
// MCParticle and LCCollectionVec functions

extern "C" LCCollectionVec* new_lccollection() {
  LCCollectionVec* mcVec = new LCCollectionVec(LCIO::MCPARTICLE);
  return mcVec;
}

extern "C" void add_particle_to_collection 
(MCParticleImpl* mcp, LCCollectionVec* mcVec) {
  mcVec->push_back( mcp );
  
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

extern "C" void lcio_particle_add_parent
( MCParticleImpl* daughter , MCParticleImpl* parent) {
  daughter->addParent( parent );
}


//////////////////////////////////////////////////////////////////////////
// LCWriter functions

extern "C" LCWriter* open_lcio_writer_new 
( char* filename, int complevel ) {
  LCWriter* lcWrt = LCFactory::getInstance()->createLCWriter();
  lcWrt->setCompressionLevel (complevel);  
  lcWrt->open( filename, LCIO::WRITE_NEW );
  return lcWrt;
}

extern "C" LCWriter* open_lcio_writer_append
( char* filename ) {
  LCWriter* lcWrt = LCFactory::getInstance()->createLCWriter();    
  lcWrt->open( filename, LCIO::WRITE_APPEND );
  return lcWrt;
}

// write the event

extern "C" LCWriter* lcio_write_event
( LCWriter* lcWrt, LCEventImpl* evt) {
  lcWrt->writeEvent( evt );
}

// destructor

extern "C" LCWriter* lcio_writer_delete ( LCWriter* lcWrt ) {
  lcWrt->close();
  delete lcWrt;
}

//////////////////////////////////////////////////////////////////////////
// LCRunHeader functions

extern "C" LCRunHeaderImpl* new_lcio_run_header( int rn ) {
  LCRunHeaderImpl* runHdr = new LCRunHeaderImpl;
  runHdr->setRunNumber (rn);
  return runHdr;
}

extern "C" void run_header_set_simstring
(LCRunHeaderImpl* runHdr, char* simstring) {
  runHdr->parameters().setValue ( "SimulationProgram", simstring );
}
    
extern "C" void write_run_header 
(LCWriter* lcWrt, const LCRunHeaderImpl* runHdr) {
  lcWrt->writeRunHeader (runHdr);
}

