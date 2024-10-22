package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valinta.kooste.AuditSession;
import fi.vm.sade.valinta.kooste.external.resource.ataru.AtaruAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.hakuapp.ApplicationAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.koski.KoskiOppija;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.OhjausparametritAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.dto.ParametritDTO;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.OppijanumerorekisteriAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto.HenkiloViiteDto;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.SuoritusrekisteriAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Oppija;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.Haku;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.TarjontaAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.ValintapisteAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.PisteetWithLastModified;
import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;
import fi.vm.sade.valinta.kooste.valintalaskenta.service.HakukohdeService;
import fi.vm.sade.valinta.kooste.valintalaskenta.service.KoskiService;
import fi.vm.sade.valinta.kooste.valintalaskenta.util.HakemuksetConverterUtil;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.SuoritustiedotDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;

@Service
public class LaskentaResurssiProvider {

  private static final Logger LOG = LoggerFactory.getLogger(LaskentaResurssiProvider.class);

  private final ValintapisteAsyncResource valintapisteAsyncResource;
  private final ApplicationAsyncResource applicationAsyncResource;
  private final AtaruAsyncResource ataruAsyncResource;
  private final ValintaperusteetAsyncResource valintaperusteetAsyncResource;
  private final SuoritusrekisteriAsyncResource suoritusrekisteriAsyncResource;
  private final OppijanumerorekisteriAsyncResource oppijanumerorekisteriAsyncResource;
  private final TarjontaAsyncResource tarjontaAsyncResource;
  private final KoskiService koskiService;
  private final HakemuksetConverterUtil hakemuksetConverterUtil;
  private final OhjausparametritAsyncResource ohjausparametritAsyncResource;
  private final HakukohdeService hakukohdeService;
  private final ExecutorService executor = Executors.newWorkStealingPool();

  @Autowired
  public LaskentaResurssiProvider(
      @Value("${valintalaskentakoostepalvelu.laskennan.splittaus:1}") int splittaus,
      ValintalaskentaResourceImpl valintalaskentaResource,
      ApplicationAsyncResource applicationAsyncResource,
      AtaruAsyncResource ataruAsyncResource,
      ValintaperusteetAsyncResource valintaperusteetAsyncResource,
      SeurantaDao seurantaDao,
      SuoritusrekisteriAsyncResource suoritusrekisteriAsyncResource,
      TarjontaAsyncResource tarjontaAsyncResource,
      ValintapisteAsyncResource valintapisteAsyncResource,
      KoskiService koskiService,
      HakemuksetConverterUtil hakemuksetConverterUtil,
      OppijanumerorekisteriAsyncResource oppijanumerorekisteriAsyncResource,
      OhjausparametritAsyncResource ohjausparametritAsyncResource,
      HakukohdeService hakukohdeService) {
    this.applicationAsyncResource = applicationAsyncResource;
    this.ataruAsyncResource = ataruAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
    this.suoritusrekisteriAsyncResource = suoritusrekisteriAsyncResource;
    this.tarjontaAsyncResource = tarjontaAsyncResource;
    this.valintapisteAsyncResource = valintapisteAsyncResource;
    this.koskiService = koskiService;
    this.hakemuksetConverterUtil = hakemuksetConverterUtil;
    this.oppijanumerorekisteriAsyncResource = oppijanumerorekisteriAsyncResource;
    this.ohjausparametritAsyncResource = ohjausparametritAsyncResource;
    this.hakukohdeService = hakukohdeService;
  }

  private CompletableFuture<LaskeDTO> getLaskeDtoFuture(
      String uuid,
      CompletableFuture<Haku> haku,
      String hakukohdeOid,
      LaskentaStartParams laskentaStartParams,
      CompletableFuture<ParametritDTO> parametritDTO,
      boolean withHakijaRyhmat,
      CompletableFuture<List<ValintaperusteetDTO>> valintaperusteetF,
      CompletableFuture<List<Oppija>> oppijatF,
      CompletableFuture<Map<String, List<String>>> hakukohdeRyhmasForHakukohdesF,
      CompletableFuture<PisteetWithLastModified> valintapisteetForHakukohdesF,
      CompletableFuture<List<ValintaperusteetHakijaryhmaDTO>> hakijaryhmatF,
      CompletableFuture<List<HakemusWrapper>> hakemuksetF,
      CompletableFuture<Map<String, KoskiOppija>> koskiOppijaByOppijaOidF) {
    return CompletableFuture.allOf(
            haku,
            parametritDTO,
            valintapisteetForHakukohdesF,
            hakijaryhmatF,
            valintaperusteetF,
            hakemuksetF,
            oppijatF,
            hakukohdeRyhmasForHakukohdesF,
            koskiOppijaByOppijaOidF)
        .thenApplyAsync(
            x -> {
              List<ValintaperusteetDTO> valintaperusteet = valintaperusteetF.join();
              verifyValintalaskentaKaytossaOrThrowError(uuid, hakukohdeOid, valintaperusteet);
              verifyJonokriteeritOrThrowError(uuid, hakukohdeOid, valintaperusteet);
              LOG.info(
                  "(Uuid: {}) Kaikki resurssit hakukohteelle {} saatu. Kootaan ja palautetaan LaskeDTO.",
                  uuid,
                  hakukohdeOid);

              Map<String, List<String>> ryhmatHakukohteittain =
                  hakukohdeRyhmasForHakukohdesF.join();
              PisteetWithLastModified pisteetWithLastModified = valintapisteetForHakukohdesF.join();
              List<HakemusWrapper> hakemukset = hakemuksetF.join();
              List<Oppija> oppijat = oppijatF.join();
              Map<String, KoskiOppija> koskiOppijatOppijanumeroittain =
                  koskiOppijaByOppijaOidF.join();
              koskiOppijatOppijanumeroittain.forEach(
                  (k, v) -> {
                    LOG.debug(String.format("Koskesta löytyi oppijalle %s datat: %s", k, v));
                  });

              if (!withHakijaRyhmat) {
                return new LaskeDTO(
                    uuid,
                    haku.join().isKorkeakouluhaku(),
                    laskentaStartParams.isErillishaku(),
                    hakukohdeOid,
                    hakemuksetConverterUtil.muodostaHakemuksetDTOfromHakemukset(
                        haku.join(),
                        hakukohdeOid,
                        ryhmatHakukohteittain,
                        hakemukset,
                        pisteetWithLastModified.valintapisteet,
                        oppijat,
                        parametritDTO.join(),
                        true,
                        true),
                    valintaperusteet);

              } else {
                return new LaskeDTO(
                    uuid,
                    haku.join().isKorkeakouluhaku(),
                    laskentaStartParams.isErillishaku(),
                    hakukohdeOid,
                    hakemuksetConverterUtil.muodostaHakemuksetDTOfromHakemukset(
                        haku.join(),
                        hakukohdeOid,
                        ryhmatHakukohteittain,
                        hakemukset,
                        pisteetWithLastModified.valintapisteet,
                        oppijat,
                        parametritDTO.join(),
                        true,
                        true),
                    valintaperusteet,
                    hakijaryhmatF.join());
              }
            });
  }

  private void verifyValintalaskentaKaytossaOrThrowError(
      String uuid, String hakukohdeOid, List<ValintaperusteetDTO> valintaperusteetList) {
    boolean jokinValintatapajonoKayttaaValintalaskentaa =
        valintaperusteetList.stream()
            .map(ValintaperusteetDTO::getValinnanVaihe)
            .flatMap(v -> v.getValintatapajono().stream())
            .anyMatch(ValintatapajonoJarjestyskriteereillaDTO::getKaytetaanValintalaskentaa);

    if (!jokinValintatapajonoKayttaaValintalaskentaa) {
      String errorMessage =
          String.format(
              "(Uuid: %s) Hakukohteen %s valittujen valinnanvaiheiden valintatapajonoissa ei käytetä valintalaskentaa, joten valintalaskentaa ei voida jatkaa ja se keskeytetään",
              uuid, hakukohdeOid);
      LOG.error(errorMessage);
      throw new RuntimeException(errorMessage);
    }
  }

  private void verifyJonokriteeritOrThrowError(
      String uuid, String hakukohdeOid, List<ValintaperusteetDTO> valintaperusteetList) {
    Predicate<? super ValintatapajonoJarjestyskriteereillaDTO>
        valintatapajonoHasPuuttuvaJonokriteeri =
        new Predicate<>() {
          @Override
          public boolean test(ValintatapajonoJarjestyskriteereillaDTO valintatapajono) {
            boolean kaytetaanValintalaskentaa = valintatapajono.getKaytetaanValintalaskentaa();
            boolean hasJarjestyskriteerit = !valintatapajono.getJarjestyskriteerit().isEmpty();

            return (kaytetaanValintalaskentaa && !hasJarjestyskriteerit)
                || (!kaytetaanValintalaskentaa && hasJarjestyskriteerit);
          }
        };
    Optional<ValintatapajonoJarjestyskriteereillaDTO>
        valintatapajonoPuutteellisellaJonokriteerilla =
        valintaperusteetList.stream()
            .map(ValintaperusteetDTO::getValinnanVaihe)
            .flatMap(v -> v.getValintatapajono().stream())
            .filter(valintatapajonoHasPuuttuvaJonokriteeri)
            .findFirst();

    if (valintatapajonoPuutteellisellaJonokriteerilla.isPresent()) {
      ValintatapajonoJarjestyskriteereillaDTO valintatapajono =
          valintatapajonoPuutteellisellaJonokriteerilla.get();
      String errorMessage =
          String.format(
              "(Uuid: %s) Hakukohteen %s valintatapajonolla %s on joko valintalaskenta ilman jonokriteereitä tai jonokriteereitä ilman valintalaskentaa, joten valintalaskentaa ei voida jatkaa ja se keskeytetään",
              uuid, hakukohdeOid, valintatapajono.getOid());
      LOG.error(errorMessage);
      throw new RuntimeException(errorMessage);
    }
  }

  public CompletableFuture<LaskeDTO> fetchResourcesForOneLaskenta(
      final String hakukohdeOid,
      AuditSession auditSession,
      LaskentaDto laskenta,
      boolean retryHakemuksetAndOppijat,
      boolean withHakijaRyhmat,
      Date nyt) {

    LaskentaStartParams laskentaStartParams = new LaskentaStartParams(
        auditSession,
        laskenta.getUuid(),
        laskenta.getHakuOid(),
        laskenta.isErillishaku(),
        fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi.VALINTARYHMA.equals(laskenta.getTyyppi()),
        laskenta.getValinnanvaihe(),
        laskenta.getValintakoelaskenta(),
        laskenta.getTyyppi());

    // TODO: tämän pitäisi olla hypernopea joten ei syytä kakutukseen
    final CompletableFuture<ParametritDTO> parametritDTOFuture = ohjausparametritAsyncResource.haeHaunOhjausparametrit(laskentaStartParams.getHakuOid());
    // TODO: tätä ei ehkä kannata hakea joka hakukohteelle uudestaan
    final CompletableFuture<Haku> hakuFuture = tarjontaAsyncResource.haeHaku(laskentaStartParams.getHakuOid());

    // TODO: tämän sisältö (tai tämä) kannattaa ehkä kakuttaa uuid:llä jottei koskesta haeta samoja oppijoita aina uudestaan
    SuoritustiedotDTO suoritustiedotDTO = new SuoritustiedotDTO();

    final String hakuOid = laskentaStartParams.getHakuOid();

    LaskentaResurssinhakuWrapper.PyynnonTunniste tunniste =
        new LaskentaResurssinhakuWrapper.PyynnonTunniste(
            "Please put individual resource source identifier here!", laskentaStartParams.getUuid(), hakukohdeOid);

    CompletableFuture<List<HakemusWrapper>> hakemukset = hakuFuture
        .thenCompose(haku -> {
          if (haku.isHakemuspalvelu()) {
            boolean haetaanHarkinnanvaraisuudet = haku.isAmmatillinenJaLukio() && haku.isKoutaHaku();
            return createResurssiFuture(
                tunniste,
                "applicationAsyncResource.getApplications",
                () ->
                    ataruAsyncResource.getApplicationsByHakukohde(
                        hakukohdeOid, haetaanHarkinnanvaraisuudet),
                retryHakemuksetAndOppijat);
          } else {
            return createResurssiFuture(
                tunniste,
                "applicationAsyncResource.getApplicationsByOid",
                () -> applicationAsyncResource.getApplicationsByOids(hakuOid, Collections.singletonList(hakukohdeOid)),
                retryHakemuksetAndOppijat);
          }
        });


    CompletableFuture<List<HenkiloViiteDto>> henkiloViitteet =
        hakemukset.thenComposeAsync(
            hws -> {
              List<HenkiloViiteDto> viitteet =
                  hws.stream()
                      .map(
                          hw ->
                              new HenkiloViiteDto(hw.getApplicationPersonOid(), hw.getPersonOid()))
                      .collect(Collectors.toList());
              return CompletableFuture.completedFuture(viitteet);
            });

    CompletableFuture<List<Oppija>> oppijasForOidsFromHakemukses =
        henkiloViitteet.thenComposeAsync(
            hws -> {
              LOG.info("Got henkiloViittees: {}", hws);
              Map<String, String> masterToOriginal =
                  hws.stream()
                      .collect(
                          Collectors.toMap(
                              HenkiloViiteDto::getMasterOid, HenkiloViiteDto::getHenkiloOid));
              List<String> oppijaOids = new ArrayList<>(masterToOriginal.keySet());
              LOG.info(
                  "Got personOids from hakemukses and getting Oppijas for these: {} for hakukohde {}",
                  oppijaOids,
                  hakukohdeOid);
              return createResurssiFuture(
                  tunniste,
                  "suoritusrekisteriAsyncResource.getSuorituksetByOppijas",
                  () ->
                      suoritusrekisteriAsyncResource.getSuorituksetByOppijas(
                          oppijaOids, hakuOid),
                  retryHakemuksetAndOppijat)
                  .thenApply(
                      oppijat -> {
                        oppijat.forEach(
                            oppija ->
                                oppija.setOppijanumero(
                                    masterToOriginal.get(oppija.getOppijanumero())));
                        return oppijat;
                      });
            });

    CompletableFuture<List<ValintaperusteetDTO>> valintaperusteet =
        createResurssiFuture(
            tunniste,
            "valintaperusteetAsyncResource.haeValintaperusteet",
            () ->
                valintaperusteetAsyncResource.haeValintaperusteet(
                    hakukohdeOid, laskentaStartParams.getValinnanvaihe()));
    CompletableFuture<Map<String, List<String>>> hakukohdeRyhmasForHakukohdes =
        createResurssiFuture(
            tunniste,
            "tarjontaAsyncResource.hakukohdeRyhmasForHakukohdes",
            () -> tarjontaAsyncResource.hakukohdeRyhmasForHakukohdes(hakuOid));
    CompletableFuture<PisteetWithLastModified> valintapisteetHakemuksille =
        hakemukset.thenComposeAsync(
            hakemusWrappers -> {
              List<String> hakemusOids =
                  hakemusWrappers.stream().map(HakemusWrapper::getOid).collect(Collectors.toList());
              return createResurssiFuture(
                  tunniste,
                  "valintapisteAsyncResource.getValintapisteetWithHakemusOidsAsFuture",
                  () ->
                      valintapisteAsyncResource.getValintapisteetWithHakemusOidsAsFuture(
                          hakemusOids, auditSession),
                  retryHakemuksetAndOppijat);
            });
    CompletableFuture<List<ValintaperusteetHakijaryhmaDTO>> hakijaryhmat =
        withHakijaRyhmat
            ? createResurssiFuture(
            tunniste,
            "valintaperusteetAsyncResource.haeHakijaryhmat",
            () -> valintaperusteetAsyncResource.haeHakijaryhmat(hakukohdeOid))
            : CompletableFuture.completedFuture(emptyList());
    CompletableFuture<Map<String, KoskiOppija>> koskiOppijaByOppijaOid =
        createResurssiFuture(
            tunniste,
            "koskiService.haeKoskiOppijat",
            () ->
                koskiService.haeKoskiOppijat(
                    hakukohdeOid, valintaperusteet, hakemukset, suoritustiedotDTO, nyt));

    LOG.info(
        "(Uuid: {}) Odotetaan kaikkien resurssihakujen valmistumista hakukohteelle {}, jotta voidaan palauttaa ne yhtenä pakettina.",
        laskentaStartParams.getUuid(),
        hakukohdeOid);
    return getLaskeDtoFuture(
        laskentaStartParams.getUuid(),
        hakuFuture,
        hakukohdeOid,
        laskentaStartParams,
        parametritDTOFuture,
        withHakijaRyhmat,
        valintaperusteet,
        oppijasForOidsFromHakemukses,
        hakukohdeRyhmasForHakukohdes,
        valintapisteetHakemuksille,
        hakijaryhmat,
        hakemukset,
        koskiOppijaByOppijaOid).thenApply(laskeDTO -> {
      laskeDTO.populoiSuoritustiedotHakemuksille(suoritustiedotDTO);
      return laskeDTO;
    });
  }

  private <T> CompletableFuture<T> createResurssiFuture(
      LaskentaResurssinhakuWrapper.PyynnonTunniste tunniste,
      String resurssi,
      Supplier<CompletableFuture<T>> sourceFuture,
      boolean retry) {
    return LaskentaResurssinhakuWrapper.luoLaskentaResurssinHakuFuture(
        sourceFuture, tunniste.withNimi(resurssi), retry);
  }

  private <T> CompletableFuture<T> createResurssiFuture(
      LaskentaResurssinhakuWrapper.PyynnonTunniste tunniste, String resurssi, Supplier<CompletableFuture<T>> sourceFuture) {
    return createResurssiFuture(tunniste, resurssi, sourceFuture, false);
  }
}
