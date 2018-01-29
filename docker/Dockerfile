ARG fromResolver=latest
FROM fullmetalhaskell/foundation:${fromResolver}

ARG resolver
RUN apt install -y context
RUN apt install -y texlive
RUN stack install ${resolver:+--resolver} $resolver pandoc

CMD ["bash"]
