FROM haskell:latest

RUN apt-get update
RUN apt install -y context
RUN apt install -y texlive
RUN stack update
RUN stack install shake
RUN stack install pandoc

CMD ["bash"]
