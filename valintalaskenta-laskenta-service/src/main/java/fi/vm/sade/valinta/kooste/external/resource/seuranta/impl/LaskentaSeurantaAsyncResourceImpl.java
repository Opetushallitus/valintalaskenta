package fi.vm.sade.valinta.kooste.external.resource.seuranta.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
import fi.vm.sade.valinta.kooste.external.resource.seuranta.LaskentaSeurantaAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.valintalaskenta.resource.LaskentaParams;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import io.reactivex.Observable;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public class LaskentaSeurantaAsyncResourceImpl implements LaskentaSeurantaAsyncResource {
  private final Logger LOG = LoggerFactory.getLogger(getClass());

  private final RestCasClient restCasClient;

  private final UrlConfiguration urlConfiguration;

  @Autowired
  public LaskentaSeurantaAsyncResourceImpl(
      @Qualifier("ValintalaskentaCasClient") RestCasClient restCasClient) {
    this.restCasClient = restCasClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  @Override
  public Observable<Optional<String>> otaSeuraavaLaskentaTyonAlle() {
    Function<String, Optional<String>> extractor =
        response -> StringUtils.isBlank(response) ? Optional.empty() : Optional.of(response);
    return Observable.fromFuture(
        this.restCasClient
            .get(
                this.urlConfiguration.url(
                    "seuranta-service.seuranta.laskenta.otaseuraavalaskentatyonalle"),
                Map.of("Accept", "text/plain"),
                10 * 60 * 1000)
            .thenApply(response -> response.getResponseBody())
            .thenApply(extractor));
  }

  public Observable<LaskentaDto> laskenta(String uuid) {
    return Observable.fromFuture(
        this.restCasClient.get(
            this.urlConfiguration.url("seuranta-service.seuranta.kuormantasaus.laskenta", uuid),
            new TypeToken<LaskentaDto>() {},
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  public Observable<LaskentaDto> resetoiTilat(String uuid) {
    return Observable.fromFuture(
        this.restCasClient.put(
            this.urlConfiguration.url(
                "seuranta-service.seuranta.kuormantasaus.laskenta.resetoi", uuid),
            new TypeToken<LaskentaDto>() {},
            uuid,
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  public Observable<TunnisteDto> luoLaskenta(
      LaskentaParams laskentaParams, List<HakukohdeDto> hakukohdeOids) {

    String url =
        this.urlConfiguration.url(
            "seuranta-service.seuranta.kuormantasaus.laskenta.tyyppi",
            laskentaParams.getHakuOid(),
            laskentaParams.getLaskentatyyppi());
    url += "?userOID=" + URLEncoder.encode(laskentaParams.getUserOID(), StandardCharsets.UTF_8);
    if (laskentaParams.getNimi() != null) {
      url += "&nimi=" + URLEncoder.encode(laskentaParams.getNimi(), StandardCharsets.UTF_8);
    }
    url += "&haunnimi=" + URLEncoder.encode(laskentaParams.getHaunNimi(), StandardCharsets.UTF_8);
    url += "&erillishaku=" + laskentaParams.isErillishaku();
    if (laskentaParams.getValinnanvaihe() != null) {
      url += "&valinnanvaihe=" + laskentaParams.getValinnanvaihe();
    }
    if (laskentaParams.getIsValintakoelaskenta() != null) {
      url += "&valintakoelaskenta=" + laskentaParams.getIsValintakoelaskenta();
    }

    return Observable.fromFuture(
        this.restCasClient.post(
            url, new TypeToken<>() {}, hakukohdeOids, Collections.emptyMap(), 10 * 60 * 1000));
  }

  public Observable<ResponseEntity> merkkaaLaskennanTila(
      String uuid, LaskentaTila tila, Optional<IlmoitusDto> ilmoitusDtoOptional) {
    String url =
        this.urlConfiguration.url(
            "seuranta-service.seuranta.kuormantasaus.laskenta.tila", uuid, tila);
    try {
      if (ilmoitusDtoOptional.isPresent()) {
        return Observable.fromFuture(
            this.restCasClient
                .post(url, ilmoitusDtoOptional.get(), Collections.emptyMap(), 10 * 60 * 1000)
                .thenApply(r -> ResponseEntity.ok().build()));
      } else {
        return Observable.fromFuture(
            this.restCasClient
                .put(url, tila, Collections.emptyMap(), 10 * 60 * 1000)
                .thenApply(r -> ResponseEntity.ok().build()));
      }
    } catch (Exception e) {
      LOG.error("Seurantapalvelun kutsu paatyi virheeseen!" + url, e);
      return Observable.error(e);
    }
  }

  public Observable<ResponseEntity> merkkaaLaskennanTila(
      String uuid,
      LaskentaTila tila,
      HakukohdeTila hakukohdetila,
      Optional<IlmoitusDto> ilmoitusDtoOptional) {
    String url =
        this.urlConfiguration.url(
            "seuranta-service.seuranta.kuormantasaus.laskenta.tila.hakukohde",
            uuid,
            tila,
            hakukohdetila);
    try {
      if (ilmoitusDtoOptional.isPresent()) {
        return Observable.fromFuture(
            this.restCasClient
                .post(url, ilmoitusDtoOptional.get(), Collections.emptyMap(), 10 * 60 * 1000)
                .thenApply(r -> ResponseEntity.ok().build()));
      } else {
        return Observable.fromFuture(
            this.restCasClient
                .put(url, new TypeToken<>() {}, tila, Collections.emptyMap(), 10 * 60 * 1000)
                .thenApply(r -> ResponseEntity.ok().build()));
      }
    } catch (Exception e) {
      LOG.error("Seurantapalvelun kutsu " + url + " laskennalle " + uuid + " paatyi virheeseen", e);
      return Observable.error(e);
    }
  }

  @Override
  public Observable<ResponseEntity> merkkaaHakukohteenTila(
      String uuid,
      String hakukohdeOid,
      HakukohdeTila tila,
      Optional<IlmoitusDto> ilmoitusDtoOptional) {
    String url =
        this.urlConfiguration.url(
            "seuranta-service.seuranta.kuormantasaus.laskenta.hakukohde.tila",
            uuid,
            hakukohdeOid,
            tila);
    try {
      if (ilmoitusDtoOptional.isPresent()) {
        return Observable.fromFuture(
            this.restCasClient
                .post(url, ilmoitusDtoOptional.get(), Collections.emptyMap(), 10 * 60 * 1000)
                .thenApply(r -> ResponseEntity.ok().build()));
      } else {
        return Observable.fromFuture(
            this.restCasClient
                .put(url, new TypeToken<>() {}, tila, Collections.emptyMap(), 10 * 60 * 1000)
                .thenApply(r -> ResponseEntity.ok().build()));
      }
    } catch (Exception e) {
      LOG.error(
          "Seurantapalvelun kutsu "
              + url
              + " laskennalle "
              + uuid
              + " ja hakukohteelle "
              + hakukohdeOid
              + " paatyi virheeseen",
          e);
      return Observable.error(e);
    }
  }
}
