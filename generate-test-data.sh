#!/bin/bash -e
# HUOM! Käynnistä sulautettu paikallinen mongodb-kanta ensin jos haluat devata palvelua paikallisesti!
#
cd valintalaskenta-laskenta-service/
mvn exec:java -Dexec.mainClass="fi.vm.sade.valintalaskenta.app.GenerateTestData" -Dexec.classpathScope="test"