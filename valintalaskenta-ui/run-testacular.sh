#!/bin/bash -e
MAVEN_OPTS="-Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n" mvn clean install org.mortbay.jetty:maven-jetty-plugin:run -Ptestacular

