# WHIZARD-GUI

## Objective

Create a GUI for WHIZARD

## Usage

Run `node app.js` (or `nodejs app.js` under Ubuntu or `npm start`) in your terminal. Then go to your favorite browser and use the program on `localhost:PORT`, where
PORT has to be specified in package.json. If no port is given, the default value is 3000.

Using with ssh: In .ssh/config, add the line "LocalForward PORT localhost:PORT" to the entry for your machine.

## Dependencies

### Packages from npm (applies for all plattforms)
`npm install express ejs`

### Ubuntu packages
Just `apt-get install npm nodejs`

### Locally from source

## Developing

Created with [Nodeclipse v0.4](https://github.com/Nodeclipse/nodeclipse-1)
 ([Eclipse Marketplace](http://marketplace.eclipse.org/content/nodeclipse), [site](http://www.nodeclipse.org))