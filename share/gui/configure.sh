#!/bin/bash

# Check Nodejs or Node or Nothing
if hash nodejs 2>/dev/null; then
	NODETYPE="nodejs"
else
	if hash node 2>/dev/null; then
		NODETYPE="node"
	fi
fi

# If Node/Nodejs installed generate script
if [ -z "$NODETYPE" ]; then 
	echo "Failure: nodejs is not found, try: sudo apt-get install nodejs"
else 
	echo "Success: '$NODETYPE' was found on your system."; 
	DAT="{\n\t\"name\": \"Whizard-GUI\",\n\t\"version\": \"0.0.1\",\n\t\"private\": true,\n\t\"scripts\": {\n\t\t\"start\": \"nodejs app.js\"\n\t},\n\t\"dependencies\": {\n\t\t\"express\": \"3.3.5\",\n\t\t\"ejs\": \"*\"\n\t}\n}"
	echo "$DAT" > package.json
	echo "Success: 'package.json' was created, you can now use: 'npm install'."
fi
