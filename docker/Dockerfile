FROM haskell:latest

RUN apt-get update
RUN stack upgrade
RUN stack update
RUN stack install shake
RUN apt install -y context
RUN apt install -y texlive
RUN stack install pandoc

CMD ["bash"]
