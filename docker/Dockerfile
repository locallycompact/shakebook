FROM haskell:latest

RUN apt-get update
RUN apt install -y pandoc 
RUN apt install -y texlive
RUN cabal update
RUN cabal install shake

CMD ["bash"]
