# WHIZARD-GUI

## Usage

Run `node server.js` (or `nodejs server.js` under Ubuntu or `npm start`) in your
terminal. Then go to your favorite browser and open `localhost:PORT`, where
PORT can be specified as environment variable `WHZ_GUI_PORT`, e.g.
`WHZ_GUI_PORT=2000 npm start`. If no port is given, the default value is 3000.

### Usage with ssh
In .ssh/config, add the line "LocalForward PORT localhost:PORT" to the entry
for your machine.

## Dependencies

### Packages from npm
`npm install` (in package directory, no arguments) will install dependencies in
local `node_modules` folder.

### Ubuntu packages
Just `apt-get install npm nodejs`.

### Locally from source
See `https://nodejs.org`.

## Developer Guidelines

### Used technologies
- EJS - EmbeddedJavaScript is the view engine for 
- Bootstrap - ?

### Directory structure
The structure is kept simple for now but might evolve more towards MVC later on:
```
├── package.json        # states the package dependencies for npm
├── server.js           # main that starts the server
├── controllers         # logic should be put here
│   ├── guiconfig.js     # user configuration
│   └── index.js          # express routes to serve sites
├── helpers             # folder for reusable js functions
│   └── utils.js          # utility functions
├── node_modules        # this is created by npm for other modules
├── output              # output folder for sindarins and runs
├── public              # static information
│   ├── css               # stylesheet
│   ├── fonts             # extra fonts
│   └── images            # used images
├── tests               # unit and integration tests
└── views               # pages that are interpreted by EJS
    ├── about.ejs
    ├── footer.ejs
    ├── header.ejs        # header loads css, fonts and sets meta data
    ├── index.ejs         # main page
    └── navbar.ejs
```
