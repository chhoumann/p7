
# How to run the dockercontainer
## WSL
Build the file using `sudo docker build -t *MYDOCKERNAME* .` and run it using `sudo docker run --env-file ./.env *MYDOCKERNAME*`

## Windows powershell
Build the file using `docker build -t *MYDOCKERNAME* .` and run it using `sudo docker run --env-file ./.env *MYDOCKERNAME*`