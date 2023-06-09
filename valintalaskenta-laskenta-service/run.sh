#!/bin/bash -e
mvn spring-boot:run -Dspring-boot.run.jvmArguments="-Dspring.profiles.active=local -Dlogging.config=file://$(pwd)/local/logback.xml"
