#!/bin/bash -e
#
# Sulautettu Mongo täytyy olla käynnissä osoitteessa localhost:37200
#
cd valintalaskenta-laskenta-service/
mvn clean install -Pembedded
cd ..
cd valintalaskenta-ui/
MAVEN_OPTS="-Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n" mvn clean install org.mortbay.jetty:maven-jetty-plugin:run -Pstartstack

