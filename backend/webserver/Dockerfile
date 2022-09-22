FROM rust:alpine
# Runs on build
# CMD runs when you call docker run
WORKDIR /api
#Copy webserver folder to container 
COPY . .
ENV PORT /PORT

CMD ["cargo", "run", "--release"] 