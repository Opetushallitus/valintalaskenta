#!/bin/bash -e
# HUOM! Käynnistä tämä sulautettu paikallinen mongodb-kanta ensin jos haluat devata palvelua paikallisesti!
#
# HUOM! Jos sulautetun mongon sulkee muualta kuin ikkunasta 
# niin prosessi ei välttämättä vapauta käytettyjä resursseja!
# 
cd valintalaskenta-service/
mvn exec:java -Dexec.mainClass="fi.vm.sade.service.valintalaskenta.service.test.app.StartEmbeddedMongo" -Dexec.classpathScope="test"