# P7
This project aimed to make a unified platform for students to solve programming exercises in Haskell.
Users would register for an account and then work on teacher-defined exercises. The exercises would be tested for successful completion automatically and the user would be able to see their progress.


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

## Database
We use Prisma. From the `frontend` folder, use `npx prisma db push` to get set up.

Docker example:
```bash
$ docker run --name postgres -e POSTGRES_PASSWORD=1234 -p "5432:5432" -d postgres
```
