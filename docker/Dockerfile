FROM haskell:latest

ARG resolver
RUN apt-get update
RUN apt-get install xz-utils make
RUN stack upgrade
RUN stack update
RUN stack setup   --resolver ${resolver} --install-ghc
RUN stack install --resolver ${resolver} shake
RUN apt install -y context
RUN apt install -y texlive
RUN stack install --resolver ${resolver} pandoc

CMD ["bash"]
