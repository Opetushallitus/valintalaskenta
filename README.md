# valintalaskenta

Palvelu suorittaa valintaperusteisiin määritellyllä logiikalla valintakoostepalvelun kautta saatavien 
hakemusten hakutoiveiden tulosten laskennan, mitä sijoittelu taas hyödyntää sijoitellessaan hakijat eri hakukohteisiin.

## Komponentit

- valintalaskenta-domain
  - Sisältää entityt
- valintalaskenta-laskenta-api
  - Sisältää rajapinnoissa käytettävät DTO:t
- valintalaskenta-laskenta-service
  - itse palvelu 
  - SpringBoot applikaatio
- valintalaskenta-tulos-service
  - Nimestä huolimatta vain osa valintalaskenta-laskenta-service palvelua
  - Sijoittelu käyttää suoraan lähdekoodin kautta tässä sijaitsevia service- ja muita luokkia

## Vaatimukset

Java 17
Maven
docker

## Palvelun lokaali ajo

Luo postgres image `docker build -t valintalaskenta-db .`

Käynnistä kontti `docker run -dp 127.0.0.1:5555:5432 valintalaskenta-db`

Käynnistä valintalaskenta-laskenta-service kansiossa `mvn spring-boot:run`

## Swagger

Löytyy esim. osoitteesta https://virkailija.testiopintopolku.fi/valintalaskenta-laskenta-service/swagger-ui/index.html

## Testaus

Repossa on sekä yksikkötestejä että integraatiotestejä (käyttävät testcontainersia postgres instanssin pyörittämiseen)

Aja kaikki testit komennolla `mvn test`


## Linttaus

Käyttää spotless maven plugaria.

Tarkista suorittamalla `mvn spotless:check`

Korjaa tyylivirheet suorittamalla `mvn spotless:apply`
