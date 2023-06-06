#!/bin/bash -e
cd valintalaskenta-laskenta-service/
mvn clean install
cd ..
cd valintalaskenta-ui/
MAVEN_OPTS="-Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n" mvn clean install spring-boot:run -Pstartstack

