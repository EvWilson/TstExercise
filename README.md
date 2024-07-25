# TstExercises

## Dependencies
- Scala 3.4.2
- sbt 1.10.1

## Usage
- You can run the first problem via `sbt "runMain Prob1"`.
- You can run the second problem via `sbt "runMain Prob2"`.
- You can run the unit tests via `sbt test`
- If you don't have Scala/sbt installed, a simple Docker workflow is also provided
    - Run `docker compose up --build problem1` and `docker compose up --build problem2` from the project root
    - These will test and then execute the respective problems
