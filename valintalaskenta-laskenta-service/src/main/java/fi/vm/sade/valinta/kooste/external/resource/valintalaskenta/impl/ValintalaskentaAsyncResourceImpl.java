package fi.vm.sade.valinta.kooste.external.resource.valintalaskenta.impl;

import static fi.vm.sade.valintalaskenta.domain.HakukohteenLaskennanTila.*;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.valintalaskenta.ValintalaskentaAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import fi.vm.sade.valinta.kooste.util.CompletableFutureUtil;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import io.reactivex.Observable;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class ValintalaskentaAsyncResourceImpl implements ValintalaskentaAsyncResource {
  private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaAsyncResourceImpl.class);
  private final int MAX_POLL_INTERVAL_IN_SECONDS = 30;
  private final RestCasClient httpclient;
  private final ExecutorService lahetaLaskeDTOExecutor = Executors.newFixedThreadPool(1);

  private final UrlConfiguration urlConfiguration;

  private final Gson gson;

  public ValintalaskentaAsyncResourceImpl(
      @Qualifier("ValintalaskentaCasClient") RestCasClient httpclient) {
    this.httpclient = httpclient;
    this.urlConfiguration = UrlConfiguration.getInstance();
    this.gson = DateDeserializer.gsonBuilder().create();
  }

  @Override
  public Observable<List<JonoDto>> jonotSijoitteluun(String hakuOid) {
    return Observable.fromFuture(
        this.httpclient.get(
            "https://"
                + this.urlConfiguration.url("host.virkailija")
                + "/valintalaskenta-laskenta-service/resources/valintatapajono/jonotsijoittelussa/"
                + hakuOid,
            new TypeToken<List<JonoDto>>() {},
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  @Override
  public CompletableFuture<List<ValintatietoValinnanvaiheDTO>> laskennantulokset(
      String hakukohdeOid) {

    return this.httpclient.get(
        this.urlConfiguration.url(
            "valintalaskenta-laskenta-service.hakukohde.valinnanvaihe", hakukohdeOid),
        new TypeToken<>() {},
        Collections.emptyMap(),
        10 * 60 * 1000);
  }

  public Observable<String> laske(LaskeDTO laskeDTO, SuoritustiedotDTO suoritustiedot) {
    Laskentakutsu laskentakutsu = new Laskentakutsu(laskeDTO, suoritustiedot);
    if (LOG.isDebugEnabled()) {
      logitaKokotiedot(laskeDTO);
      logitaSuoritustietojenKoko(suoritustiedot);
      LOG.debug(
          String.format(
              "Suoritustietojen koko base64-gzippinä: %d",
              laskentakutsu.getSuoritustiedotDtoBase64Gzip().length()));
    }
    try {
      return kutsuRajapintaaPollaten(
          "valintalaskenta-laskenta-service.valintalaskenta.laske", laskentakutsu);
    } catch (Exception e) {
      throw e;
    }
  }

  @Override
  public Observable<String> valintakokeet(LaskeDTO laskeDTO, SuoritustiedotDTO suoritustiedot) {
    Laskentakutsu laskentakutsu = new Laskentakutsu(laskeDTO, suoritustiedot);
    if (LOG.isDebugEnabled()) {
      logitaKokotiedot(laskeDTO);
      logitaSuoritustietojenKoko(suoritustiedot);
      LOG.debug(
          String.format(
              "Suoritustietojen koko base64-gzippinä: %d",
              laskentakutsu.getSuoritustiedotDtoBase64Gzip().length()));
    }
    try {
      return kutsuRajapintaaPollaten(
          "valintalaskenta-laskenta-service.valintalaskenta.valintakokeet", laskentakutsu);
    } catch (Exception e) {
      throw e;
    }
  }

  @Override
  public Observable<String> laskeKaikki(LaskeDTO laskeDTO, SuoritustiedotDTO suoritustiedot) {
    Laskentakutsu laskentakutsu = new Laskentakutsu(laskeDTO, suoritustiedot);
    if (LOG.isDebugEnabled()) {
      logitaKokotiedot(laskeDTO);
      logitaSuoritustietojenKoko(suoritustiedot);
      LOG.debug(
          String.format(
              "Suoritustietojen koko base64-gzippinä: %d",
              laskentakutsu.getSuoritustiedotDtoBase64Gzip().length()));
    }
    try {
      return kutsuRajapintaaPollaten(
          "valintalaskenta-laskenta-service.valintalaskenta.laskekaikki", laskentakutsu);
    } catch (Exception e) {
      throw e;
    }
  }

  @Override
  public Observable<String> laskeJaSijoittele(
      String uuid, List<LaskeDTO> lista, SuoritustiedotDTO suoritustiedot) {
    Laskentakutsu laskentakutsu =
        Laskentakutsu.luoTyhjaValintaryhmaLaskentaPalasissaSiirtoaVarten(uuid);
    if (LOG.isDebugEnabled()) {
      lista.forEach(this::logitaKokotiedot);
      logitaSuoritustietojenKoko(suoritustiedot);
    }

    LOG.info(
        String.format(
            "Laskenta %s : siirretään %d hakukohteen laskennan käynnistys paloissa valintalaskennalle.",
            laskentakutsu.getUuid(), lista.size()));

    try {
      return kutsuRajapintaaPollaten(
          laskentakutsu,
          (aloitaLaskentakutsunLahettaminenPaloissa(laskentakutsu)
              .thenComposeAsync(
                  x ->
                      CompletableFutureUtil.sequence(
                          lista.stream()
                              .map(dto -> lahetaYksittainenLaskeDto(dto, laskentakutsu))
                              .collect(Collectors.toList())))
              .thenComposeAsync(x -> lahetaSuoritustiedot(suoritustiedot, laskentakutsu))
              .thenComposeAsync(
                  x -> {
                    LOG.info(
                        String.format(
                            "Laskenta %s : kaikkien %d hakukohteen laskentaresurssit on saatu siirrettyä paloissa valintalaskennalle. Lähetetään käynnistyskutsu!",
                            laskentakutsu.getUuid(), lista.size()));
                    return kaynnistaPaloissaSiirrettyLaskenta(laskentakutsu);
                  })));
    } catch (Exception e) {
      LOG.error("Valintaryhmälaskennan käynnistyksessä tapahtui virhe", e);
      throw e;
    }
  }

  private CompletableFuture<String> aloitaLaskentakutsunLahettaminenPaloissa(
      Laskentakutsu laskentakutsu) {
    final String url =
        this.urlConfiguration.url(
            "valintalaskenta-laskenta-service.valintalaskenta.aloita.laskentakutsu.paloissa",
            laskentakutsu.getPollKey());
    return httpclient
        .post(
            url,
            laskentakutsu,
            Map.of("Content-Type", "application/json", "Accept", "text/plain"),
            60 * 1000)
        .thenApply(response -> response.getResponseBody())
        .whenComplete(
            (tulos, poikkeus) -> {
              if (poikkeus != null) {
                LOG.error(
                    String.format(
                        "Laskenta %s : Virhe lähetettäessä paloissa siirrettävän laskentakutsun aloitusta valintalaskennalle osoitteeseen '%s'",
                        laskentakutsu.getUuid(), url),
                    poikkeus);
              } else {
                LOG.info(
                    String.format(
                        "Laskenta %s : Saatiin valintalaskennalta paloissa siirrettävän laskentakutsun aloitukseen vastaus '%s'",
                        laskentakutsu.getUuid(), tulos));
              }
            });
  }

  private CompletableFuture<String> lahetaYksittainenLaskeDto(
      LaskeDTO dto, Laskentakutsu laskentakutsu) {
    return CompletableFuture.supplyAsync(
        () -> {
          final String url =
              this.urlConfiguration.url(
                  "valintalaskenta-laskenta-service.valintalaskenta.lisaa.hakukohde.laskentakutsuun",
                  laskentakutsu.getPollKey());

          return httpclient
              .post(
                  url,
                  dto,
                  Map.of("Content-Type", "application/json", "Accept", "text/plain"),
                  10 * 60 * 1000)
              .thenApply(response -> response.getResponseBody())
              .whenComplete(
                  (tulos, poikkeus) -> {
                    if (poikkeus != null) {
                      LOG.error(
                          String.format(
                              "Laskenta %s , hakukohde %s : Virhe lähetettäessä hakukohteen lisäämistä kutsuun valintalaskennalle osoitteeseen '%s'",
                              laskentakutsu.getUuid(), dto.getHakukohdeOid(), url),
                          poikkeus);
                    } else {
                      LOG.info(
                          String.format(
                              "Laskenta %s , hakukohde %s : Saatiin valintalaskennalta hakukohteen lisäämiseen vastaus '%s'",
                              laskentakutsu.getUuid(), dto.getHakukohdeOid(), tulos));
                    }
                  })
              .join();
        },
        lahetaLaskeDTOExecutor);
  }

  private CompletableFuture<String> lahetaSuoritustiedot(
      SuoritustiedotDTO suoritustiedot, Laskentakutsu laskentakutsu) {
    final String url =
        this.urlConfiguration.url(
            "valintalaskenta-laskenta-service.valintalaskenta.lisaa.suoritustiedot.laskentakutsuun",
            laskentakutsu.getPollKey());
    return httpclient
        .postPlaintext(
            url,
            Laskentakutsu.toBase64Gzip(suoritustiedot),
            Map.of("Content-Type", "text/plain", "Accept", "text/plain"),
            10 * 60 * 1000)
        .thenApply(response -> response.getResponseBody())
        .whenComplete(
            (tulos, poikkeus) -> {
              if (poikkeus != null) {
                LOG.error(
                    String.format(
                        "Laskenta %s : Virhe lähetettäessä suoritustietojen lisäämistä kutsuun valintalaskennalle osoitteeseen '%s'",
                        laskentakutsu.getUuid(), url),
                    poikkeus);
              } else {
                LOG.info(
                    String.format(
                        "Laskenta %s : Saatiin valintalaskennalta suoritustietojen lisäämiseen vastaus '%s'",
                        laskentakutsu.getUuid(), tulos));
              }
            });
  }

  private CompletableFuture<String> kaynnistaPaloissaSiirrettyLaskenta(
      Laskentakutsu laskentakutsu) {
    final String url =
        this.urlConfiguration.url(
            "valintalaskenta-laskenta-service.valintalaskenta.kaynnista.paloissa.aloitettu.laskenta",
            laskentakutsu.getPollKey());
    return httpclient
        .post(
            url,
            laskentakutsu,
            Map.of("Content-Type", "application/json", "Accept", "text/plain"),
            10 * 60 * 1000)
        .thenApply(response -> response.getResponseBody())
        .whenComplete(
            (tulos, poikkeus) -> {
              if (poikkeus != null) {
                LOG.error(
                    String.format(
                        "Laskenta %s : Virhe lähetettäessä paloissa siirretyn laskentakutsun aloitusta valintalaskennalle osoitteeseen '%s'",
                        laskentakutsu.getUuid(), url),
                    poikkeus);
              } else {
                LOG.info(
                    String.format(
                        "Laskenta %s : Saatiin valintalaskennalta paloissa siirretyn laskennan aloitukseen vastaus '%s'",
                        laskentakutsu.getUuid(), tulos));
              }
            });
  }

  @Override
  public CompletableFuture<ValinnanvaiheDTO> lisaaTuloksia(
      String hakuOid, String hakukohdeOid, String tarjoajaOid, ValinnanvaiheDTO vaihe) {
    HashMap<String, String> query = new HashMap<>();
    query.put("tarjoajaOid", tarjoajaOid);
    return this.httpclient.post(
        this.urlConfiguration.url(
            "valintalaskenta-laskenta-service.hakukohde.valinnanvaihe", hakukohdeOid, query),
        new TypeToken<ValinnanvaiheDTO>() {},
        vaihe,
        Collections.emptyMap(),
        5 * 60 * 1000);
  }

  public Observable<String> pollaa(int pollInterval, Object result, String uuid, String pollKey) {
    if (VALMIS.equals(result)) {
      return Observable.just(VALMIS);
    } else if (VIRHE.equals(result)) {
      LOG.error("Virhe laskennan suorituksessa, lopetetaan");
      return Observable.error(
          new RuntimeException(String.format("(Pollkey: %s) Laskenta epäonnistui!", pollKey)));
    } else {
      if (pollInterval == MAX_POLL_INTERVAL_IN_SECONDS) {
        LOG.warn(
            String.format("(Pollkey=%s) Laskenta kestää pitkään! Jatketaan pollausta.", pollKey));
      }
      return Observable.timer(pollInterval, TimeUnit.SECONDS)
          .switchMap(
              d -> {
                String url =
                    this.urlConfiguration.url(
                        "valintalaskenta-laskenta-service.valintalaskenta.status", pollKey);
                return Observable.fromFuture(
                        this.httpclient
                            .get(url, Map.of("Accept", "text/plain"), 10 * 60 * 1000)
                            .thenApply(response -> response.getResponseBody()))
                    .switchMap(
                        rval ->
                            pollaa(
                                Math.min(pollInterval * 2, MAX_POLL_INTERVAL_IN_SECONDS),
                                rval,
                                uuid,
                                pollKey));
              });
    }
  }

  public Observable<String> kutsuRajapintaaPollaten(String api, Laskentakutsu laskentakutsu) {
    LOG.info(
        "(Uuid: {}) Lähetetään laskenta-servicelle laskentakutsu. (Pollkey: {})",
        laskentakutsu.getUuid(),
        laskentakutsu.getPollKey());

    CompletableFuture<String> requestFuture =
        this.httpclient
            .post(
                this.urlConfiguration.url(api),
                laskentakutsu,
                Map.of("Content-Type", "application/json", "Accept", "text/plain"),
                10 * 60 * 1000)
            .thenApply(response -> response.getResponseBody());

    return kutsuRajapintaaPollaten(laskentakutsu, requestFuture);
  }

  private Observable<String> kutsuRajapintaaPollaten(
      Laskentakutsu laskentakutsu, CompletableFuture<String> requestFuture) {
    return Observable.fromFuture(requestFuture)
        .switchMap(
            rval -> {
              if (UUSI.equals(rval)) {
                LOG.info(
                    "Saatiin tieto, että uusi laskenta on luotu (Pollkey: {}). Pollataan sen tilaa kunnes se on päättynyt (VALMIS tai VIRHE).",
                    laskentakutsu.getPollKey());
                return pollaa(1, rval, laskentakutsu.getUuid(), laskentakutsu.getPollKey());
              } else {
                LOG.error(
                    "Yritettiin käynnistää laskenta, mutta saatiin palautusarvona {} eikä UUSI. Pollauksen pitäisi olla käynnissä muualla. Ei pollata.",
                    rval);
                return Observable.error(
                    new RuntimeException(
                        String.format(
                            "Laskenta (pollKey=%s) epäonnistui!", laskentakutsu.getPollKey())));
              }
            });
  }

  private void logitaKokotiedot(LaskeDTO laskeDTO) {
    Function<Object, Integer> koonLaskenta = o -> this.gson.toJson(o).length();
    try {
      LOG.debug(
          String.format(
              "laskeDTO %s (hakukohde %s) koot: %s",
              laskeDTO.getUuid(),
              laskeDTO.getHakukohdeOid(),
              laskeDTO.logSerializedSizes(koonLaskenta)));
    } catch (Exception e) {
      LOG.error(
          String.format(
              "Virhe, kun yritettiin logittaa laskeDTO:n %s (hakukohde %s) kokoa",
              laskeDTO.getUuid(), laskeDTO.getHakukohdeOid()),
          e);
    }
  }

  private void logitaSuoritustietojenKoko(SuoritustiedotDTO suoritustiedotDTO) {
    try {
      LOG.debug(
          String.format("Suoritustietojen koko: %s", this.gson.toJson(suoritustiedotDTO).length()));
    } catch (Exception e) {
      LOG.error("Virhe, kun yritettiin logittaa suoritustietojen kokoa", e);
    }
  }

  @Override
  public Observable<List<ValintatietoValinnanvaiheDTO>> hakukohde(String hakukohdeoid) {
    return Observable.fromFuture(
        this.httpclient.get(
            this.urlConfiguration.url(
                "valintalaskenta-laskenta-service.hakukohde.valinnanvaihe", hakukohdeoid),
            new TypeToken<List<ValintatietoValinnanvaiheDTO>>() {},
            Collections.emptyMap(),
            10 * 60 * 1000));
  }
}
