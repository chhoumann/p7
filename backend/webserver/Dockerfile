FROM rust
# Runs on build
# CMD runs when you call docker run
WORKDIR /api
#Copy webserver folder to container 
COPY . .
ENV PORT=/PORT
RUN echo PORT

RUN apk add rustup
RUN apk add gcc
RUN apk add build-base

ENV PATH=#{user-home-abs-path}/.cargo/bin:"$PATH"

CMD ["cargo", "run", "--release"] 