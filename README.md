# P7

## Backend setup
_The following commands you are in the `backend/webserver` folder._

Build the file using:
```bash
$ docker build -t webserver .
```
_Builds dockerfile in current directory and names it `webserver`._

and run it using
```bash
$ docker run -p 8000:8000 -e PORT=8000 -e ROCKET_ADDRESS=0.0.0.0 webserver
```
_Runs the `webserver` image given port and environment variables. Port and address environment variables are necessary._

You may need to prepend `sudo`, depending on your user permissions / OS.

## Frontend setup
_The following commands you are in the `frontend` folder._

To run the frontend, ensure you have `npm` installed. Then run:
```bash
$ npm ci
```

This installs the latest packages for the project.

Then run:
```bash
$ npm run dev
```
to enter development mode. 

This may fail if you do not have the appropriate environment variables set. You can set them in a `.env` file in the `frontend` folder. The variables are:
```bash
WEBSERVER_ADDRESS=http://localhost:8000
```
Which depends on the port you set for the backend.