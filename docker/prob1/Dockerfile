FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.10.1_3.4.2 as test

COPY project/ project/
COPY src/ src/
COPY build.sbt build.sbt

RUN sbt test

CMD sbt "runMain Prob1"
