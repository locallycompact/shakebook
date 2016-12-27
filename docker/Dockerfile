FROM haskell:latest

RUN apt-get update
RUN apt install -y pandoc
RUN cabal update
RUN cabal install shake

CMD ["bash"]
