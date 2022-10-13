
# How to run the docker container
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