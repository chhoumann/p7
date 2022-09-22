FROM rust:alpine
# Runs on build
# CMD runs when you call docker run
WORKDIR /api
#Copy webserver folder to container 
COPY . .
ENV PORT=/PORT
RUN echo PORT

apk add rustup
apk add gcc
apk add build-base

CMD ["cargo", "run", "--release"] 