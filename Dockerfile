# Copied from https://github.com/icfpcontest2020/dockerfiles/blob/aa83929530dcc6a4b59fefc5f276cc5429fcaf82/dockerfiles/haskell/Dockerfile

FROM icfpcontest2020/haskell

WORKDIR /solution
COPY . .
RUN chmod +x ./build.sh
RUN chmod +x ./run.sh
RUN ./build.sh
ENTRYPOINT ["./run.sh"]
