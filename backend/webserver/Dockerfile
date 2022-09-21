FROM rust:alpine
# Runs on build
# CMD runs when you call docker run
WORKDIR /api
#Copy webserver folder to container 
COPY . .
CMD ["cargo", "run", "--release"] 