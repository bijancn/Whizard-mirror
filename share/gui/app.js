
/**
 * Module dependencies.
 */

var express = require('express');
var routes = require('./routes');
var user = require('./routes/user');
var guiconf = require('./public/js/gui.config');
var environment = process.env;

var http = require('http');
var path = require('path');
var fs = require('fs'); // file storing
var app = express();

var bodyParser = require('body-parser');
var errorHandler = require('error-handler');
var favicon = require('favicon');
var methodOverwrite = require('express-method-override');
var router = require('app-router');

/*
 * Functions in use
 */ 
var mkdirSync = function (path) {
  try {
    fs.mkdirSync(path);
  } catch(e) {
    if ( e.code != 'EEXIST' ) throw e;
  }
}

/*
 * Logical variables
 */
var DEFAULT_PORT = guiconf.PORT;
var WhizRunning = false; 
  
// all environments
// ***Setup the port which is to be used.
// Take care that if no port if specified in the json-script, or if the app 
// is started not using npm but node, the default-port is used.
if (typeof(environment.USE_PORT) !== "undefined") {
	app.set('port', process.env.PORT || environment.USE_PORT);
} else {
	app.set('port', process.env.PORT || DEFAULT_PORT);
}
app.set('views', __dirname + '/views');
app.set('view engine', 'ejs');
app.use(favicon);
app.use (bodyParser.urlencoded({
	extended: true
}));
app.use(methodOverwrite);
app.use(router);
app.use(express.static(path.join(__dirname, 'public')));
app.use(express.static(path.join(__dirname, 'output-whiz')));


app.use(errorHandler);

/*
 *  Accessibility control
 */ 
app.get('/', routes.index);
app.get('/users', user.list);

var about = require('./routes/about');
app.get('/about', about.about);


http.createServer(app).listen(app.get('port'), function(){
  console.log('Express server listening on port ' + app.get('port'));
  console.log('Access GUI: http://localhost:' + app.get('port'));
});

if (guiconf.DEBUG)
	console.log('[ Debug mode is On ]');
/*
 * Attempt to create work flow folders
 */ 
mkdirSync(guiconf.WHIZARD_OUTPUT_DIR);
mkdirSync(guiconf.WHIZARD_OUTPUT_SIN);

/* 
 * Button: Run Sindarin
 */ 
app.post('/runwhiz', function (req, res) {
    console.log('Run Whizard clicked.');
    
    fs.writeFile("output/gui-generated", req.body.src, function(err) {
		if(err) {
			return console.log(err);
		}

		console.log("The file was saved!");
	}); 
	
	var exec = require('child_process').exec,
		child;

	//child = exec('cd output-whiz && ' + environment.WHIZ_BIN + '/whizard -r ../output/gui-generated',
	WhizRunning = true;
	
	if (guiconf.DEBUG)
		console.log('Executing: cd ' + guiconf.WHIZARD_OUTPUT_DIR + ' && whizard '+ req.body.option +' ../output/gui-generated');
	
	child = exec('cd ' + guiconf.WHIZARD_OUTPUT_DIR + ' && whizard '+ req.body.option +' ../output/gui-generated',

    function (error, stdout, stderr) {
        if(stdout!==''){
            console.log('---------stdout: ---------\n' + stdout);
        }
        if(stderr!==''){
            console.log('---------stderr: ---------\n' + stderr);
        }
        if (error !== null) {
            console.log('---------exec error: ---------\n[' + error+']');
        }
        WhizRunning = false;
        res.end('finished');
    });
});



/* 
 * Button: Save Sindarin
 */ 
app.post('/savesin', function (req, res) {
    console.log('Save Sindarin clicked.');
    
    fs.writeFile(guiconf.WHIZARD_OUTPUT_DIR + "gui-generated", req.body.src, function(err) {
		if(err) {
			res.end('Error saving.');
			return console.log(err);
		}

		console.log("The file was saved!");
		res.end('Saved Succesfully.');
	}); 
	
});


/* 
 * Action: Check file timestamp
 */ 
app.post('/checktimestamp', function (req, res) {
    
    fs.stat(req.body.filename, function(err, stats) {
		if(err) {
			res.end('Could not read filestamp.');
			return console.log(err);
		}

		//console.log(req.body.filename);
		res.end(stats.mtime + '');
	}); 
	
});
