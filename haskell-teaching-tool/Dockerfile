FROM haskell:8.8.4

RUN cabal v2-update

WORKDIR /usr/src/haskell

COPY haskell-teaching-tool.cabal .

RUN cabal v2-build --dependencies-only

# COPY CHANGELOG.md .
COPY LICENSE      .
COPY src          ./src

RUN cabal v2-build haskell-teaching-tool
RUN cp `find . -type f -name haskell-teaching-tool` .

COPY frontend ./frontend

CMD ["./haskell-teaching-tool", "+RTS", "-N4"]
