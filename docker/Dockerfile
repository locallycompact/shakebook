FROM haskell:latest

RUN apt-get update
RUN apt install -y context
RUN apt install -y texlive
RUN cabal update
RUN cabal install shake
RUN stack install pandoc

CMD ["bash"]
