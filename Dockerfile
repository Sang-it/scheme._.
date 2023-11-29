FROM haskell:9.4.8

WORKDIR /app
ADD . /app

RUN cabal update
RUN cabal build
RUN cabal install --installdir=.

ENTRYPOINT ["./scheme-in48"]
