var MGUI = {};
(function(context) { 
	/*
	 * Directory to output whizard files
	 */ 	
    context.WHIZARD_OUTPUT_DIR = "output-whiz/";
    
    /*
	 * Directory to store sindarin file
	 */ 
    context.WHIZARD_OUTPUT_SIN = "output/";
    
    /*
	 * Default port
	 */    
    context.PORT = "3000";
    
    /*
	 * Time in ms to update histograms/log file while whizard is running.
	 */ 
    context.UPDATE_TIME = 10000;
    
    /*
     * Use Google Chart API for Latex images
     * Internet connection is required.
     */
     context.USE_GOOGLE_LATEX = true;
      
    /*
     * Debug mode outputs a lot of things in the console (for Development)
     */ 
    context.DEBUG = true;
 	
})(MGUI);  


/* 
 * Exporting data to nodejs
 */ 
if(typeof module == "undefined"){
    module = this;
}

module.exports = {
  WHIZARD_OUTPUT_DIR: MGUI.WHIZARD_OUTPUT_DIR,
  
  WHIZARD_OUTPUT_SIN: MGUI.WHIZARD_OUTPUT_SIN,
  
  UPDATE_TIME: MGUI.UPDATE_TIME,
  
  PORT: MGUI.PORT,
  
  USE_GOOGLE_LATEX: MGUI.USE_GOOGLE_LATEX,
  
  DEBUG: MGUI.DEBUG
};
