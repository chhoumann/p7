FROM rust
# Runs on build
# CMD runs when you call docker run
WORKDIR /api
#Copy webserver folder to container 
COPY . .
ENV PORT=/PORT
RUN echo PORT

CMD ["cargo", "run", "--release"] 