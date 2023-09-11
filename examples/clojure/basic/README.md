### To run development environment
``` bash
clj -A:dev:cider
```


### Build and run uberjar
``` bash
clj -T:build release
java -cp dev:target/eywa.0.1.0.jar example.main
```
Add dev directory to classpath so logback.xml is on classpath
If not set logging default logging level is DEBUG and log appender
is ConsoleAppender. This will overload termminal with large log
output and slow down application. Keep this in mind...
