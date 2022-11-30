FROM rust:latest

workdir /api
COPY ./backend/webserver-rocket/. .

# Install GHC and other dependencies. Source: maintainer of GHCup, as seen below.
# https://stackoverflow.com/questions/67680726/installing-haskells-cabal-or-ghcup-inside-a-dockerfile-wont-work
ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/Berlin

# install dependencies
RUN \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
        curl \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        gcc-multilib \
        autoconf \
        automake \
        build-essential

# install gpg keys
ARG GPG_KEY=7784930957807690A66EBDBE3786C5262ECB4A3F
RUN gpg --batch --keyserver keys.openpgp.org --recv-keys $GPG_KEY

# install ghcup
RUN \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup && \
    ghcup config set gpg-setting GPGStrict

ARG GHC=8.10.7
ARG CABAL=latest

# Install GHC and cabal
RUN \
    ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}

RUN cabal update && cabal install --package-env=. --lib hspec hspec-contrib QuickCheck HUnit

RUN ["cargo", "build","--release", "--verbose", "--jobs", "1"]

CMD ["./target/release/web_server"]
