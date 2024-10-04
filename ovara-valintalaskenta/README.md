# ovara-valintalaskenta #

Erillinen moduuli siirtotiedostojen ajastetulle luomiselle. Main-luokka OvaraApp etsii käynnistyessään
sovelluksen kannasta viimeisimmän onnistuneen siirtotiedostojen muodostuksen aikaikkunan loppuhetken.
Uusi aikaikkuna määritellään operaation alkaessa edellisen onnistuneen aikaikkunan lopusta nykyhetkeen.

Jos muuttuneita tietoja on aikavälillä paljon (kts. konffiarvo xyz), muodostuu useita tiedostoja.

Muodostetut tiedostot tallennetaan sovellukselle konffattuun s3-ämpäriin seuraavien konffiarvojen perusteella:
suoritusrekisteri.ovara.s3.region
suoritusrekisteri.ovara.s3.bucket
suoritusrekisteri.ovara.s3.target-role-arn

Sovelluksen ajoympäristö kts. cloud-base -> ovara-generic-stack.ts.

## Ajoympäristöjen versiot

- Java 17

## Ajaminen lokaalisti

Sovelluksen lokaali ajaminen vaatii sopivan kannan tai putkituksen sellaiseen, tarvittaessa 
ovara-valintalaskenta/src/main/resources/ovara-application.yml kanssa yhteensopivan tyhjän kannan saa pystyyn tähän tapaan:
``docker run --rm --name valintalaskenta-db -p 5555:5432 -e POSTGRES_USER=app -e POSTGRES_PASSWORD=app -e POSTGRES_DB=valintalaskenta -d postgres:15``

Käynnistetään ajamalla OvaraApp-luokka. Tämän voi tehdä joko IDEstä (katso alta tarvittavat konffi- ja profiiliparametrit kuntoon)
tai ajamalla projektin juuresta suoraan ovara-valintalaskennan spring boot-jaria suoraan esimerkiksi näin:
``mvn clean install``
``java -Dspring.config.additional-location=ovara-valintalaskenta/src/main/resources/ovara-application.yml -Dspring.profiles.active=ovara -jar ovara-valintalaskenta/target/ovara-valintaperusteet.jar``

 