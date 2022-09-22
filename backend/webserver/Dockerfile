FROM rust

workdir /api
COPY . .

# https://github.com/judge0/compilers/blob/master/Dockerfile
# Check for latest version here: https://www.haskell.org/ghc/download.html
ENV HASKELL_VERSIONS \
      8.8.1
RUN set -xe && \
    apt-get update && \
    apt-get install -y --no-install-recommends libgmp-dev libtinfo5 && \
    rm -rf /var/lib/apt/lists/* && \
    for VERSION in $HASKELL_VERSIONS; do \
      curl -fSsL "https://downloads.haskell.org/~ghc/$VERSION/ghc-$VERSION-x86_64-deb8-linux.tar.xz" -o /tmp/ghc-$VERSION.tar.xz && \
      mkdir /tmp/ghc-$VERSION && \
      tar -xf /tmp/ghc-$VERSION.tar.xz -C /tmp/ghc-$VERSION --strip-components=1 && \
      rm /tmp/ghc-$VERSION.tar.xz && \
      cd /tmp/ghc-$VERSION && \
      ./configure \
        --prefix=/usr/local/ghc-$VERSION && \
      make -j$(nproc) install && \
      rm -rf /tmp/*; \
    done

CMD ["cargo", "run"]