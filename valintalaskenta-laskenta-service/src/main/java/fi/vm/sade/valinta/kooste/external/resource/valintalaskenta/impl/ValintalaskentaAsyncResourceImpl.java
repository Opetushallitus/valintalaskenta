package fi.vm.sade.valinta.kooste.external.resource.valintalaskenta.impl;

import com.google.gson.Gson;
import fi.vm.sade.valinta.kooste.external.resource.valintalaskenta.ValintalaskentaAsyncResource;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import io.reactivex.Observable;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/*
Varmista että edelleen pätee  @PreAuthorize(OPH_CRUD)
 */
@Service
public class ValintalaskentaAsyncResourceImpl implements ValintalaskentaAsyncResource {
  private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaAsyncResourceImpl.class);
  private final Gson gson;

  @Autowired ValintalaskentaResourceImpl valintalaskentaResource;

  public ValintalaskentaAsyncResourceImpl() {
    this.gson = DateDeserializer.gsonBuilder().create();
  }

  @Override
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
      return Observable.fromFuture(
          CompletableFuture.completedFuture(valintalaskentaResource.laske(laskentakutsu)));
    } catch (Exception e) {
      throw new RuntimeException(e);
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
      return Observable.fromFuture(
          CompletableFuture.completedFuture(valintalaskentaResource.valintakokeet(laskentakutsu)));
    } catch (Exception e) {
      throw new RuntimeException(e);
      // throw e;
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
      return Observable.fromFuture(
          CompletableFuture.completedFuture(valintalaskentaResource.laskeKaikki(laskentakutsu)));
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Tiksussa b15df0500 Merge remote-tracking branch
   * 'origin/VTKU-181__valintaryhmalaskennan_kutsu_pienempiin_paloihin' tämän kutsun siirto
   * valintalaskentakoostepalvelusta valintalaskentaan oli palasteltu moneen kutsuun. Kun kutsu ei
   * enää mene verkon yli ei (käsittääkseni) ole enää mitää syytä palastella joten palatta takaisin
   * yhteen kutsuun.
   */
  @Override
  public Observable<String> laskeJaSijoittele(
      String uuid, List<LaskeDTO> lista, SuoritustiedotDTO suoritustiedot) {
    Laskentakutsu laskentakutsu = new Laskentakutsu(lista, suoritustiedot);
    if (LOG.isDebugEnabled()) {
      lista.forEach(this::logitaKokotiedot);
      logitaSuoritustietojenKoko(suoritustiedot);
      LOG.debug(
          String.format(
              "Suoritustietojen koko base64-gzippinä: %d",
              laskentakutsu.getSuoritustiedotDtoBase64Gzip().length()));
    }

    try {
      return Observable.fromFuture(
          CompletableFuture.completedFuture(
              valintalaskentaResource.laskeJaSijoittele(laskentakutsu)));
    } catch (Exception e) {
      throw e;
    }
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
}
