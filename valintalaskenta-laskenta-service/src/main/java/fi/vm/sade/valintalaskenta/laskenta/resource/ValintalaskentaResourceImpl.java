package fi.vm.sade.valintalaskenta.laskenta.resource;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO;
import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import fi.vm.sade.valintalaskenta.domain.HakukohteenLaskennanTila;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.Laskentakutsu;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StopWatch;
import org.springframework.web.bind.annotation.*;

@Component
public class ValintalaskentaResourceImpl {
  private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaResourceImpl.class);

  private final ValintalaskentaService valintalaskentaService;
  private final ValisijoitteluKasittelija valisijoitteluKasittelija;
  private final ValiSijoitteluResource valiSijoitteluResource;
  private final ErillisSijoitteluResource erillisSijoitteluResource;
  private final ValintaperusteetValintatapajonoResource valintatapajonoResource;
  private final ExecutorService executorService;

  private static volatile ConcurrentHashMap<String, String> hakukohteetLaskettavina;

  @Autowired
  public ValintalaskentaResourceImpl(
      ValintalaskentaService valintalaskentaService,
      ValisijoitteluKasittelija valisijoitteluKasittelija,
      ValiSijoitteluResource valiSijoitteluResource,
      ErillisSijoitteluResource erillisSijoitteluResource,
      ValintaperusteetValintatapajonoResource valintatapajonoResource,
      @Value("${valintalaskenta-laskenta-service.parallelism:1}") int parallelismFromConfig) {
    this.valintalaskentaService = valintalaskentaService;
    this.valisijoitteluKasittelija = valisijoitteluKasittelija;
    this.valiSijoitteluResource = valiSijoitteluResource;
    this.erillisSijoitteluResource = erillisSijoitteluResource;
    this.valintatapajonoResource = valintatapajonoResource;

    hakukohteetLaskettavina = new ConcurrentHashMap<>();

    this.executorService =
        Executors.newWorkStealingPool(
            Math.max(Runtime.getRuntime().availableProcessors(), parallelismFromConfig));
  }

  public String status(@PathVariable("key") final String pollKey) throws Exception {
    try {
      return pidaKirjaaMeneillaanOlevista(pollKey, false);
    } catch (Exception e) {
      LOG.error("Odottamaton virhe : pidaKirjaaMeneillaanOlevista", e);
      throw e;
    }
  }

  public String laske(@RequestBody final Laskentakutsu laskentakutsu) throws Exception {
    try {
      String pollKey = laskentakutsu.getPollKey();
      laskentakutsu.populoiSuoritustiedotLaskeDtoille();
      String status = pidaKirjaaMeneillaanOlevista(pollKey);
      if (!status.equals(HakukohteenLaskennanTila.UUSI)) {
        return status;
      } else {
        // Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran samalle
        // tunnisteelle vaikka pyyntöjä tulisi useita
        try {
          executorService.submit(
              timeRunnable.apply(laskentakutsu.getUuid(), () -> toteutaLaskenta(laskentakutsu)));
          return HakukohteenLaskennanTila.UUSI;
        } catch (Exception e) {
          LOG.error("Virhe laskennan suorituksessa, ", e);
          throw e;
        }
      }
    } catch (Exception e) {
      LOG.error("Odottamaton virhe : laske(Laskentakutsu laskentakutsu)", e);
      throw e;
    }
  }

  public String valintakokeet(@RequestBody final Laskentakutsu laskentakutsu) throws Exception {
    try {
      String pollKey = laskentakutsu.getPollKey();
      laskentakutsu.populoiSuoritustiedotLaskeDtoille();
      String status = pidaKirjaaMeneillaanOlevista(pollKey);
      if (!status.equals(HakukohteenLaskennanTila.UUSI)) {
        return status;
      } else {
        // Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran samalle
        // tunnisteelle vaikka pyyntöjä tulisi useita
        try {
          executorService.submit(
              timeRunnable.apply(
                  laskentakutsu.getUuid(), () -> toteutaValintakoeLaskenta(laskentakutsu)));
          return HakukohteenLaskennanTila.UUSI;
        } catch (Exception e) {
          LOG.error("Virhe laskennan suorituksessa, ", e);
          throw e;
        }
      }
    } catch (Exception e) {
      LOG.error("Odottamaton virhe: valintakokeet(Laskentakutsu laskentakutsu)", e);
      throw e;
    }
  }

  public String laskeKaikki(@RequestBody final Laskentakutsu laskentakutsu) throws Exception {
    try {
      String pollKey = laskentakutsu.getPollKey();
      laskentakutsu.populoiSuoritustiedotLaskeDtoille();
      String status = pidaKirjaaMeneillaanOlevista(pollKey);
      if (!status.equals(HakukohteenLaskennanTila.UUSI)) {
        return status;
      } else {
        // Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran samalle
        // tunnisteelle vaikka pyyntöjä tulisi useita
        try {
          executorService.submit(
              timeRunnable.apply(laskentakutsu.getUuid(), () -> toteutaLaskeKaikki(laskentakutsu)));
          return HakukohteenLaskennanTila.UUSI;
        } catch (Exception e) {
          LOG.error("Virhe laskennan suorituksessa, ", e);
          throw e;
        }
      }
    } catch (Exception e) {
      LOG.error("Odottamaton virhe: laskeKaikki(Laskentakutsu laskentakutsu)", e);
      throw e;
    }
  }

  public String laskeJaSijoittele(@RequestBody final Laskentakutsu laskentakutsu) {
    try {
      laskentakutsu.populoiSuoritustiedotLaskeDtoille();
      List<LaskeDTO> lista = laskentakutsu.getLaskeDTOs();
      String pollKey = laskentakutsu.getPollKey();
      if (lista == null || lista.isEmpty()) {
        LOG.error(
            "Laskejasijoittele-rajapinta sai syötteeksi tyhjän listan, joten laskentaa ei voida toteuttaa. lopetetaan.");
        return HakukohteenLaskennanTila.VIRHE;
      }
      String status = pidaKirjaaMeneillaanOlevista(pollKey);
      if (!status.equals(HakukohteenLaskennanTila.UUSI)) {
        return status;
      } else {
        // Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran samalle
        // tunnisteelle vaikka pyyntöjä tulisi useita
        try {
          executorService.submit(
              timeRunnable.apply(
                  laskentakutsu.getUuid(), () -> toteutaLaskeJaSijoittele(laskentakutsu)));
          return HakukohteenLaskennanTila.UUSI;
        } catch (Exception e) {
          LOG.error("Virhe laskennan suorituksessa, ", e);
          throw e;
        }
      }
    } catch (Exception e) {
      LOG.error("Odottamaton virhe: laskeJaSijoittele(Laskentakutsu laskentakutsu) ", e);
      throw e;
    }
  }

  private void erillisSijoittele(
      LaskeDTO laskeDTO, ValintaperusteetDTO valintaperusteetDTO, StopWatch stopWatch) {
    stopWatch.start("Suoritetaan erillissijoittelu");
    List<String> jonot =
        valintaperusteetDTO.getValinnanVaihe().getValintatapajono().stream()
            .map(ValintatapajonoJarjestyskriteereillaDTO::getOid)
            .collect(Collectors.toList());
    Map<String, List<String>> kohteet = new HashMap<>();
    kohteet.put(valintaperusteetDTO.getHakukohdeOid(), jonot);
    stopWatch.stop();
    erillissijoitteleJonot(laskeDTO, kohteet, stopWatch);
  }

  private Map<String, List<String>> haeKopiotValintaperusteista(List<String> jonot) {
    return valintatapajonoResource.findKopiot(jonot);
  }

  private void valisijoitteleKopiot(
      LaskeDTO laskeDTO, Map<String, List<String>> valisijoiteltavatJonot, StopWatch stopWatch) {
    stopWatch.start("Suoritetaan välisijoittelu " + valisijoiteltavatJonot.size() + " jonolle");
    String hakuoid = getHakuOid(laskeDTO);
    ValisijoitteluDTO dto = new ValisijoitteluDTO();
    dto.setHakukohteet(valisijoiteltavatJonot);
    List<HakukohdeDTO> sijoitellut = valiSijoitteluResource.sijoittele(hakuoid, dto);

    Map<String, HakemusDTO> hakemusHashMap = new ConcurrentHashMap<>();

    // Muodostetaan Map hakemuksittain sijoittelun tuloksista
    sijoitellut.parallelStream()
        .forEach(
            hakukohde ->
                hakukohde.getValintatapajonot().parallelStream()
                    .forEach(
                        valintatapajono ->
                            valintatapajono.getHakemukset().parallelStream()
                                .forEach(
                                    hakemus ->
                                        hakemusHashMap.put(
                                            hakukohde.getOid()
                                                + valintatapajono.getOid()
                                                + hakemus.getHakemusOid(),
                                            hakemus))));

    valintalaskentaService.applyValisijoittelu(valisijoiteltavatJonot, hakemusHashMap);
    stopWatch.stop();
  }

  private String getHakuOid(final LaskeDTO laskeDTO) {
    String hakuoid;
    try {
      hakuoid = laskeDTO.getValintaperuste().get(0).getHakuOid();
    } catch (Exception e) {
      LOG.error("Välisijoittelulle ei löytynyt hakuoidia! uuid=" + laskeDTO.getUuid(), e);
      throw e;
    }
    return hakuoid;
  }

  private void erillissijoitteleJonot(
      LaskeDTO laskeDTO, Map<String, List<String>> jonot, StopWatch stopWatch) {
    stopWatch.start("Suoritetaan erillissijoittelu " + jonot.size() + " jonolle");
    String hakuoid;
    try {
      hakuoid = laskeDTO.getValintaperuste().get(0).getHakuOid();
    } catch (Exception e) {
      LOG.error("Erillissijoittelulle ei löytynyt hakuoidia! uuid=" + laskeDTO.getUuid(), e);
      if (stopWatch.isRunning()) {
        stopWatch.stop();
      }
      throw e;
    }
    ValisijoitteluDTO dto = new ValisijoitteluDTO();
    dto.setHakukohteet(jonot);
    Long ajo = erillisSijoitteluResource.sijoittele(hakuoid, dto);
    if (ajo == null) {
      stopWatch.stop();
      throw new RuntimeException(
          "Erillissijoittelu haulle "
              + hakuoid
              + " näyttää epäonnistuneen, koska rajapinta palautti null");
    }
    valintalaskentaService.applyErillissijoittelu(jonot, ajo);
    stopWatch.stop();
  }

  private String pidaKirjaaMeneillaanOlevista(String pollKey) {
    return pidaKirjaaMeneillaanOlevista(pollKey, true);
  }

  private String pidaKirjaaMeneillaanOlevista(String pollKey, boolean luoJosPuuttuu) {
    if (!hakukohteetLaskettavina.containsKey(pollKey)) {
      if (luoJosPuuttuu) {
        LOG.info(String.format("Luodaan uusi laskettava hakukohde. %s", pollKey));
        ForkJoinPool forkJoinPool = (ForkJoinPool) this.executorService;
        LOG.info(
            String.format(
                "Parallelism %d, pool size %d",
                forkJoinPool.getParallelism(), forkJoinPool.getPoolSize()));
        hakukohteetLaskettavina.put(pollKey, HakukohteenLaskennanTila.KESKEN);
        return HakukohteenLaskennanTila.UUSI;
      } else {
        LOG.error(
            String.format(
                "Haettiin statusta laskennalle %s, mutta laskentaa ei ole olemassa. Onko palvelin käynnistetty välissä uudelleen?",
                pollKey));
        return HakukohteenLaskennanTila.VIRHE;
      }
    } else if (hakukohteetLaskettavina.get(pollKey).equals(HakukohteenLaskennanTila.VALMIS)) {
      LOG.info(String.format("Kohteen laskenta on valmistunut. %s", pollKey));
      // hakukohteetLaskettavina.remove(key); //!! Joissain tilanteissa käynnistetään tarpeettomasti
      // uusi laskenta
      return HakukohteenLaskennanTila.VALMIS;
    } else if (hakukohteetLaskettavina.get(pollKey).equals(HakukohteenLaskennanTila.VIRHE)) {
      LOG.error(String.format("Kohteen laskennassa on tapahtunut virhe. %s", pollKey));
      return HakukohteenLaskennanTila.VIRHE;
    } else {
      LOG.info(String.format("Hakukohteen laskenta on edelleen kesken. %s", pollKey));
      return HakukohteenLaskennanTila.KESKEN;
    }
  }

  // Tilaa päivitetään vain, jos se on tällä hetkellä KESKEN.
  private void paivitaKohteenLaskennanTila(String pollKey, String tila, StopWatch stopWatch) {
    stopWatch.start("Päivitetään kohteen laskenta tilaan: " + tila);
    if (hakukohteetLaskettavina.containsKey(pollKey)) {
      if (hakukohteetLaskettavina.get(pollKey).equals(HakukohteenLaskennanTila.KESKEN)) {
        if (tila.equals(HakukohteenLaskennanTila.VALMIS)) {
          LOG.info("Ollaan valmiita, merkataan kohteen laskennan tila valmiiksi. {} {}", pollKey);
        }
        hakukohteetLaskettavina.replace(pollKey, tila);
      } else {
        LOG.error(
            "(Pollkey: {}) Yritettiin päivittää sellaisen laskennan tilaa, joka ei ollut tilassa KESKEN. Ei päivitetty. tila nyt: {} ",
            pollKey,
            hakukohteetLaskettavina.get(pollKey));
      }
    } else {
      LOG.error(
          "(Pollkey: {}) Yritettiin päivittää sellaisen laskennan tilaa, jota ei vaikuta olevan olemassa.",
          pollKey);
    }
    stopWatch.stop();
  }

  private boolean isErillisHaku(LaskeDTO laskeDTO, ValintaperusteetDTO valintaperusteetDTO) {
    return laskeDTO.isErillishaku()
        && valintaperusteetDTO.getViimeinenValinnanvaihe()
            == valintaperusteetDTO.getValinnanVaihe().getValinnanVaiheJarjestysluku();
  }

  private void toteutaLaskenta(Laskentakutsu laskentakutsu) {
    LaskeDTO laskeDTO = laskentakutsu.getLaskeDTO();
    String pollkey = laskentakutsu.getPollKey();

    StopWatch stopWatch =
        new StopWatch(
            "Toteutetaan laskenta laskentakutsulle "
                + laskentakutsu.getUuid()
                + " hakukohteessa "
                + laskeDTO.getHakukohdeOid());
    LOG.info(
        String.format(
            "(Uuid=%s) Aloitetaan laskenta X hakukohteessa %s",
            laskeDTO.getUuid(), laskeDTO.getHakukohdeOid()));
    try {
      ValisijoitteluKasittelija.ValisijoiteltavatJonot valisijoiteltavatJonot =
          valisijoitteluKasittelija.valisijoiteltavatJonot(
              Collections.singletonList(laskeDTO), stopWatch);
      if (!valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
        valisijoiteltavatJonot =
            new ValisijoitteluKasittelija.ValisijoiteltavatJonot(
                valisijoiteltavatJonot.valinnanvaiheet,
                haeKopiotValintaperusteista(
                    valisijoiteltavatJonot.jonot.get(laskeDTO.getHakukohdeOid())));
      }
      ValintaperusteetDTO valintaperusteetDTO = laskeDTO.getValintaperuste().get(0);
      stopWatch.start("Tarkistetaan, onko haku erillishaku");
      boolean erillisHaku = isErillisHaku(laskeDTO, valintaperusteetDTO);
      stopWatch.stop();
      if (erillisHaku) {
        setSijoittelunKayttamanKentat(valintaperusteetDTO, stopWatch);
      }
      stopWatch.start(
          "Suoritetaan laskenta "
              + laskeDTO.getUuid()
              + ". Hakemuksia "
              + laskeDTO.getHakemus().size()
              + " ja valintaperusteita "
              + laskeDTO.getValintaperuste().size()
              + " kpl.");
      LOG.info(
          String.format(
              "(Uuid=%s) Suoritetaan laskenta. Hakemuksia %s kpl ja valintaperusteita %s kpl",
              laskeDTO.getUuid(),
              laskeDTO.getHakemus().size(),
              laskeDTO.getValintaperuste().size()));
      valintalaskentaService.laske(
          laskeDTO.getHakemus(),
          laskeDTO.getValintaperuste(),
          laskeDTO.getHakijaryhmat(),
          laskeDTO.getHakukohdeOid(),
          laskeDTO.getUuid(),
          laskeDTO.isKorkeakouluhaku());
      stopWatch.stop();
      if (erillisHaku) {
        erillisSijoittele(laskeDTO, valintaperusteetDTO, stopWatch);
      }
      LOG.info(
          String.format(
              "(Uuid=%s) Laskenta suoritettu hakukohteessa %s",
              laskeDTO.getUuid(), laskeDTO.getHakukohdeOid()));
      if (!valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
        valisijoitteleKopiot(laskeDTO, valisijoiteltavatJonot.jonot, stopWatch);
      }

      stopWatch.start(
          "Siivotaan poistuneisiin valinnanvaiheisiin liittyneet valintakoeosallistumiset");
      valintalaskentaService.siivoaValintakoeOsallistumisetPuuttuviltaValinnanvaiheilta(
          Collections.singletonList(laskeDTO));
      stopWatch.stop();

      paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VALMIS, stopWatch);
      LOG.info(stopWatch.prettyPrint());
    } catch (Exception e) {
      if (stopWatch.isRunning()) {
        stopWatch.stop();
      }
      LOG.error(String.format("Valintalaskenta epaonnistui! uuid=%s", laskeDTO.getUuid()), e);
      paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VIRHE, stopWatch);
      LOG.info(stopWatch.prettyPrint());
    }
  }

  private void toteutaValintakoeLaskenta(Laskentakutsu laskentakutsu) {
    StopWatch stopWatch =
        new StopWatch("Toteutetaan valintakoelaskenta laskentakutsulle " + laskentakutsu.getUuid());
    LaskeDTO laskeDTO = laskentakutsu.getLaskeDTO();
    String pollkey = laskentakutsu.getPollKey();

    try {
      stopWatch.start(
          "Suoritetaan valintakoelaskenta "
              + laskeDTO.getUuid()
              + " "
              + laskeDTO.getHakemus().size()
              + " hakemukselle hakukohteessa"
              + laskeDTO.getHakukohdeOid());
      LOG.info(
          String.format(
              "(Uuid=%s) Suoritetaan valintakoelaskenta %s hakemukselle hakukohteessa %s",
              laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getHakukohdeOid()));
      valintalaskentaService.valintakokeetRinnakkain(
          laskeDTO.getHakemus(),
          laskeDTO.getValintaperuste(),
          laskeDTO.getUuid(),
          new ValintakoelaskennanKumulatiivisetTulokset(),
          laskeDTO.isKorkeakouluhaku());
      LOG.info(
          String.format(
              "(Uuid=%s) Valintakoelaskenta suoritettu %s hakemukselle hakukohteessa %s",
              laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getHakukohdeOid()));
      stopWatch.stop();

      stopWatch.start(
          "Siivotaan poistuneisiin valinnanvaiheisiin liittyneet valintakoeosallistumiset");
      valintalaskentaService.siivoaValintakoeOsallistumisetPuuttuviltaValinnanvaiheilta(
          Collections.singletonList(laskeDTO));
      stopWatch.stop();

      paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VALMIS, stopWatch);
      LOG.info(stopWatch.prettyPrint());
    } catch (Exception e) {
      LOG.error(String.format("Valintakoelaskenta epaonnistui! uuid=%s", laskeDTO.getUuid()), e);
      if (stopWatch.isRunning()) {
        stopWatch.stop();
      }
      paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VIRHE, stopWatch);
      LOG.info(stopWatch.prettyPrint());
    }
  }

  public void toteutaLaskeKaikki(Laskentakutsu laskentakutsu) {
    LaskeDTO laskeDTO = laskentakutsu.getLaskeDTO();
    String pollkey = laskentakutsu.getPollKey();

    StopWatch stopWatch =
        new StopWatch(
            "Suoritetaan valintakoelaskenta "
                + laskeDTO.getUuid()
                + " hakukohteessa "
                + laskeDTO.getHakukohdeOid());
    LOG.info(
        String.format(
            "(Uuid=%s) Aloitetaan laskenta hakukohteessa %s",
            laskeDTO.getUuid(), laskeDTO.getHakukohdeOid()));
    try {
      ValisijoitteluKasittelija.ValisijoiteltavatJonot valisijoiteltavatJonot =
          valisijoitteluKasittelija.valisijoiteltavatJonot(
              Collections.singletonList(laskeDTO), stopWatch);

      if (valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
        if (laskeDTO.isErillishaku()) {
          laskeDTO
              .getValintaperuste()
              .forEach(
                  v -> {
                    if (v.getValinnanVaihe().getValinnanVaiheJarjestysluku()
                        == v.getViimeinenValinnanvaihe()) {
                      setSijoittelunKayttamanKentat(v, stopWatch);
                    }
                  });
        }
        stopWatch.start("Lasketaan kaikki " + laskeDTO.getHakemus().size() + " hakemusta");
        valintalaskentaService.laskeKaikki(
            laskeDTO.getHakemus(),
            laskeDTO.getValintaperuste(),
            laskeDTO.getHakijaryhmat(),
            laskeDTO.getHakukohdeOid(),
            laskeDTO.getUuid(),
            laskeDTO.isKorkeakouluhaku());
        stopWatch.stop();
        if (laskeDTO.isErillishaku()) {
          laskeDTO.getValintaperuste().stream()
              .filter(
                  v ->
                      v.getValinnanVaihe().getValinnanVaiheJarjestysluku()
                          == v.getViimeinenValinnanvaihe())
              .forEach(v -> erillisSijoittele(laskeDTO, v, stopWatch));
        }
      } else {
        stopWatch.start("Muodostetaan valinnanvaihe map");
        Map<Integer, List<LaskeDTO>> map = new TreeMap<>();
        laskeDTO
            .getValintaperuste()
            .forEach(
                v -> {
                  List<LaskeDTO> dtos =
                      map.getOrDefault(
                          v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), new ArrayList<>());
                  dtos.add(
                      new LaskeDTO(
                          laskeDTO.getUuid(),
                          laskeDTO.isKorkeakouluhaku(),
                          laskeDTO.isErillishaku(),
                          laskeDTO.getHakukohdeOid(),
                          laskeDTO.getHakemus(),
                          Collections.singletonList(v),
                          laskeDTO.getHakijaryhmat()));
                  map.put(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), dtos);
                });
        stopWatch.stop();

        ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset =
            new ValintakoelaskennanKumulatiivisetTulokset();
        map.keySet().stream()
            .forEachOrdered(
                key -> {
                  map.get(key)
                      .forEach(
                          dto -> {
                            ValintaperusteetValinnanVaiheDTO valinnanVaihe =
                                dto.getValintaperuste().get(0).getValinnanVaihe();
                            if (valinnanVaihe
                                .getValinnanVaiheTyyppi()
                                .equals(ValinnanVaiheTyyppi.VALINTAKOE)) {
                              LOG.info(
                                  String.format(
                                      "(Uuid=%s) Suoritetaan valintakoelaskenta %s hakemukselle",
                                      laskeDTO.getUuid(), laskeDTO.getHakemus().size()));

                              stopWatch.start(
                                  "Suoritetaan laskenta"
                                      + laskeDTO.getHakemus().size()
                                      + " hakemukselle ");
                              valintalaskentaService.valintakokeetRinnakkain(
                                  laskeDTO.getHakemus(),
                                  dto.getValintaperuste(),
                                  laskeDTO.getUuid(),
                                  kumulatiivisetTulokset,
                                  laskeDTO.isKorkeakouluhaku());
                              stopWatch.stop();
                            } else {
                              ValintaperusteetDTO valintaperusteetDTO =
                                  dto.getValintaperuste().get(0);
                              boolean erillisHaku = isErillisHaku(laskeDTO, valintaperusteetDTO);

                              if (erillisHaku) {
                                // Aseta sijoittelun käyttämät kentät
                                setSijoittelunKayttamanKentat(valintaperusteetDTO, stopWatch);
                              }
                              LOG.info(
                                  String.format(
                                      "(Uuid=%s) Suoritetaan laskenta. Hakemuksia %s kpl ja valintaperusteita %s kpl",
                                      laskeDTO.getUuid(),
                                      laskeDTO.getHakemus().size(),
                                      laskeDTO.getValintaperuste().size()));
                              stopWatch.start(
                                  "Suoritetaan laskenta"
                                      + laskeDTO.getHakemus().size()
                                      + " hakemukselle ");
                              valintalaskentaService.laske(
                                  dto.getHakemus(),
                                  dto.getValintaperuste(),
                                  dto.getHakijaryhmat(),
                                  dto.getHakukohdeOid(),
                                  laskeDTO.getUuid(),
                                  laskeDTO.isKorkeakouluhaku());
                              stopWatch.stop();

                              if (erillisHaku) {
                                erillisSijoittele(dto, valintaperusteetDTO, stopWatch);
                              }
                            }
                          });
                  if (valisijoiteltavatJonot.valinnanvaiheet.contains(key)) {
                    valisijoitteleKopiot(
                        laskeDTO,
                        new ImmutablePair<>(
                                valisijoiteltavatJonot.valinnanvaiheet,
                                haeKopiotValintaperusteista(
                                    valisijoiteltavatJonot.jonot.get(laskeDTO.getHakukohdeOid())))
                            .getRight(),
                        stopWatch);
                  }
                });
      }

      stopWatch.start(
          "Siivotaan poistuneisiin valinnanvaiheisiin liittyneet valintakoeosallistumiset");
      valintalaskentaService.siivoaValintakoeOsallistumisetPuuttuviltaValinnanvaiheilta(
          Collections.singletonList(laskeDTO));
      stopWatch.stop();

      LOG.info(
          String.format(
              "(Uuid=%s) Laskenta suoritettu hakukohteessa %s",
              laskeDTO.getUuid(), laskeDTO.getHakukohdeOid()));
      paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VALMIS, stopWatch);
      LOG.info(stopWatch.prettyPrint());
    } catch (Exception e) {
      LOG.error(
          String.format(
              "Valintalaskenta ja valintakoelaskenta epaonnistui! uuid=%s", laskeDTO.getUuid()),
          e);
      if (stopWatch.isRunning()) {
        stopWatch.stop();
      }

      paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VIRHE, stopWatch);
      LOG.info(stopWatch.prettyPrint());
    }
  }

  private void toteutaLaskeJaSijoittele(Laskentakutsu laskentakutsu) {
    List<LaskeDTO> lista = laskentakutsu.getLaskeDTOs();
    String pollKey = laskentakutsu.getPollKey();
    StopWatch stopWatch =
        new StopWatch("Suoritetaan valintaryhmälaskenta uuid:lla " + lista.get(0).getUuid());
    LOG.info(String.format("Aloitetaan valintaryhmälaskenta uuid:lla %s", lista.get(0).getUuid()));
    try {
      ValisijoitteluKasittelija.ValisijoiteltavatJonot valisijoiteltavatJonot =
          valisijoitteluKasittelija.valisijoiteltavatJonot(lista, stopWatch);
      if (valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
        stopWatch.start(
            "Suoritetaan valintalaskenta ilman sijoittelujonoja " + lista.size() + " hakemukselle");
        lista.forEach(
            laskeDTO ->
                valintalaskentaService.laskeKaikki(
                    laskeDTO.getHakemus(),
                    laskeDTO.getValintaperuste(),
                    laskeDTO.getHakijaryhmat(),
                    laskeDTO.getHakukohdeOid(),
                    laskeDTO.getUuid(),
                    laskeDTO.isKorkeakouluhaku()));
        stopWatch.stop();
      } else {
        stopWatch.start("Muodostetaan lista vaiheittain laskettavista hakukohteista");
        Map<Integer, List<LaskeDTO>> laskettavatHakukohteetVaiheittain = new TreeMap<>();
        lista.forEach(
            laskeDTO ->
                laskeDTO
                    .getValintaperuste()
                    .forEach(
                        v -> {
                          List<LaskeDTO> dtos =
                              laskettavatHakukohteetVaiheittain.getOrDefault(
                                  v.getValinnanVaihe().getValinnanVaiheJarjestysluku(),
                                  new ArrayList<>());
                          dtos.add(
                              new LaskeDTO(
                                  laskeDTO.getUuid(),
                                  laskeDTO.isKorkeakouluhaku(),
                                  laskeDTO.isErillishaku(),
                                  laskeDTO.getHakukohdeOid(),
                                  laskeDTO.getHakemus(),
                                  Collections.singletonList(v),
                                  laskeDTO.getHakijaryhmat()));
                          laskettavatHakukohteetVaiheittain.put(
                              v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), dtos);
                        }));
        stopWatch.stop();
        final int valinnanVaiheidenMaara = laskettavatHakukohteetVaiheittain.size();
        ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset =
            new ValintakoelaskennanKumulatiivisetTulokset();
        laskettavatHakukohteetVaiheittain.keySet().stream()
            .forEachOrdered(
                vaiheenJarjestysNumero -> {
                  List<LaskeDTO> laskePerValinnanvaihe =
                      laskettavatHakukohteetVaiheittain.get(vaiheenJarjestysNumero);
                  final int hakukohteidenMaaraValinnanVaiheessa = laskePerValinnanvaihe.size();
                  for (int i = 0; i < laskePerValinnanvaihe.size(); ++i) { // indeksin vuoksi
                    LaskeDTO laskeDTO = laskePerValinnanvaihe.get(i);
                    final ValintaperusteetDTO valintaPerusteet =
                        laskeDTO.getValintaperuste().get(0);
                    ValintaperusteetValinnanVaiheDTO valinnanVaihe =
                        valintaPerusteet.getValinnanVaihe();
                    try {
                      if (valinnanVaihe
                          .getValinnanVaiheTyyppi()
                          .equals(ValinnanVaiheTyyppi.VALINTAKOE)) {
                        LOG.info(
                            String.format(
                                "(Uuid=%s, %s=%s/%s, hakukohde=%s/%s) Suoritetaan valintakoelaskenta %s hakemukselle",
                                laskeDTO.getUuid(),
                                valinnanVaihe.getValinnanVaiheTyyppi(),
                                vaiheenJarjestysNumero,
                                valinnanVaiheidenMaara,
                                i + 1,
                                hakukohteidenMaaraValinnanVaiheessa,
                                laskeDTO.getHakemus().size()));
                        stopWatch.start(
                            "Suoritetaan valintakoelaskenta Uuid:n "
                                + laskeDTO.getUuid()
                                + " "
                                + laskeDTO.getHakemus().size()
                                + " hakemukselle");
                        valintalaskentaService.valintakokeetRinnakkain(
                            laskeDTO.getHakemus(),
                            laskeDTO.getValintaperuste(),
                            laskeDTO.getUuid(),
                            kumulatiivisetTulokset,
                            laskeDTO.isKorkeakouluhaku());
                        stopWatch.stop();
                      } else {
                        stopWatch.start("Tarkistetaan, onko haku erillishaku");
                        boolean erillisHaku = isErillisHaku(laskeDTO, valintaPerusteet);
                        stopWatch.stop();
                        if (erillisHaku) {
                          // Aseta sijoittelun käyttämät kentät
                          setSijoittelunKayttamanKentat(valintaPerusteet, stopWatch);
                        }
                        LOG.info(
                            String.format(
                                "(Uuid=%s, %s=%s/%s, hakukohde=%s/%s) Suoritetaan laskenta %s hakemukselle",
                                laskeDTO.getUuid(),
                                valinnanVaihe.getValinnanVaiheTyyppi(),
                                vaiheenJarjestysNumero,
                                valinnanVaiheidenMaara,
                                i + 1,
                                hakukohteidenMaaraValinnanVaiheessa,
                                laskeDTO.getHakemus().size()));
                        stopWatch.start(
                            "Suoritetaan valintalaskenta "
                                + laskeDTO.getHakemus().size()
                                + " hakemukselle");
                        valintalaskentaService.laske(
                            laskeDTO.getHakemus(),
                            laskeDTO.getValintaperuste(),
                            laskeDTO.getHakijaryhmat(),
                            laskeDTO.getHakukohdeOid(),
                            laskeDTO.getUuid(),
                            laskeDTO.isKorkeakouluhaku());
                        stopWatch.stop();
                        if (valisijoiteltavatJonot.valinnanvaiheet.contains(
                            vaiheenJarjestysNumero)) {
                          Map<String, List<String>> kohteet = valisijoiteltavatJonot.jonot;
                          if (kohteet.containsKey(laskeDTO.getHakukohdeOid())) {

                            List<String> jonot = kohteet.get(laskeDTO.getHakukohdeOid());
                            LOG.info(
                                String.format(
                                    "(Uuid=%s, %s=%s/%s, hakukohde=%s/%s) Suoritetaan välisijoittelu hakukohteelle %s",
                                    laskeDTO.getUuid(),
                                    valinnanVaihe.getValinnanVaiheTyyppi(),
                                    vaiheenJarjestysNumero,
                                    valinnanVaiheidenMaara,
                                    i + 1,
                                    hakukohteidenMaaraValinnanVaiheessa,
                                    laskeDTO.getHakukohdeOid()));
                            valisijoitteleKopiot(
                                laskeDTO, haeKopiotValintaperusteista(jonot), stopWatch);
                          }
                        }

                        if (erillisHaku) {
                          erillisSijoittele(laskeDTO, valintaPerusteet, stopWatch);
                        }
                      }

                      LOG.info(
                          String.format(
                              "(Uuid=%s, %s=%s/%s, hakukohde=%s/%s) Laskenta suoritettu hakukohteessa %s",
                              laskeDTO.getUuid(),
                              valinnanVaihe.getValinnanVaiheTyyppi(),
                              vaiheenJarjestysNumero,
                              valinnanVaiheidenMaara,
                              i + 1,
                              hakukohteidenMaaraValinnanVaiheessa,
                              laskeDTO.getHakukohdeOid()));
                    } catch (Throwable t) {
                      if (stopWatch.isRunning()) {
                        stopWatch.stop();
                      }
                      paivitaKohteenLaskennanTila(
                          pollKey, HakukohteenLaskennanTila.VIRHE, stopWatch);
                      LOG.error(
                          String.format(
                              "(Uuid=%s, %s=%s/%s, hakukohde=%s/%s) virhe hakukohteelle %s",
                              laskeDTO.getUuid(),
                              valinnanVaihe.getValinnanVaiheTyyppi(),
                              vaiheenJarjestysNumero,
                              valinnanVaiheidenMaara,
                              i + 1,
                              hakukohteidenMaaraValinnanVaiheessa,
                              laskeDTO.getHakukohdeOid()),
                          t);
                      LOG.info(stopWatch.prettyPrint());
                      throw new RuntimeException(t);
                    }
                  }
                });
      }
      if (stopWatch.isRunning()) {
        stopWatch.stop();
      }

      stopWatch.start(
          "Siivotaan poistuneisiin valinnanvaiheisiin liittyneet valintakoeosallistumiset");
      valintalaskentaService.siivoaValintakoeOsallistumisetPuuttuviltaValinnanvaiheilta(lista);
      stopWatch.stop();

      paivitaKohteenLaskennanTila(pollKey, HakukohteenLaskennanTila.VALMIS, stopWatch);
      LOG.info(stopWatch.prettyPrint());
    } catch (Throwable t) {
      LOG.error(
          "Valintaryhmälaskennassa tapahtui yllättävä virhe. Lopetetaan ja merkitään laskenta virheelliseksi. ",
          t);
      if (stopWatch.isRunning()) {
        stopWatch.stop();
      }
      paivitaKohteenLaskennanTila(pollKey, HakukohteenLaskennanTila.VIRHE, stopWatch);
      LOG.info(stopWatch.prettyPrint());
    }
  }

  private void setSijoittelunKayttamanKentat(ValintaperusteetDTO v, StopWatch stopWatch) {
    stopWatch.start("Asetetaan sijoittelun käyttämät kentät");
    v.getValinnanVaihe()
        .getValintatapajono()
        .forEach(
            j -> {
              j.setSiirretaanSijoitteluun(true);
              j.setValmisSijoiteltavaksi(true);
            });
    stopWatch.stop();
  }

  private BiFunction<String, Runnable, Runnable> timeRunnable =
      (uuid, r) ->
          () -> {
            long start = System.currentTimeMillis();
            try {
              r.run();
            } catch (Throwable t) {
              long end = System.currentTimeMillis();
              LOG.error(
                  String.format(
                      "(Uuid=%s) (Kesto %ss) Odottamaton virhe.",
                      uuid, millisToString(end - start)),
                  t);
              throw t;
            } finally {
              long end = System.currentTimeMillis();
              LOG.info(
                  String.format(
                      "(Uuid=%s) (Kesto %ss) Laskenta valmis!", uuid, millisToString(end - start)));
            }
          };

  private static String millisToString(long millis) {
    return new BigDecimal(millis)
        .divide(new BigDecimal(1000), 2, BigDecimal.ROUND_HALF_UP)
        .toPlainString();
  }
}
