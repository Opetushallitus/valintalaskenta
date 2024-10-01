package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import static java.util.Collections.emptyList;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valinta.kooste.AuditSession;
import fi.vm.sade.valinta.kooste.KoosteAudit;
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
import fi.vm.sade.valinta.kooste.valintalaskenta.actor.LaskentaResurssinhakuWrapper.PyynnonTunniste;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;
import fi.vm.sade.valinta.kooste.valintalaskenta.service.HakukohdeService;
import fi.vm.sade.valinta.kooste.valintalaskenta.service.KoskiService;
import fi.vm.sade.valinta.kooste.valintalaskenta.util.HakemuksetConverterUtil;
import fi.vm.sade.valinta.sharedutils.AuditLog;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.SuoritustiedotDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Service;

@Service
@ManagedResource(
    objectName = "OPH:name=LaskentaActorFactory",
    description = "LaskentaActorFactory mbean")
public class LaskentaActorFactory {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaActorFactory.class);

  private final ValintapisteAsyncResource valintapisteAsyncResource;
  private final ValintalaskentaResourceImpl valintalaskentaResource;
  private final ApplicationAsyncResource applicationAsyncResource;
  private final AtaruAsyncResource ataruAsyncResource;
  private final ValintaperusteetAsyncResource valintaperusteetAsyncResource;
  private final SeurantaDao seurantaDao;
  private final SuoritusrekisteriAsyncResource suoritusrekisteriAsyncResource;
  private final OppijanumerorekisteriAsyncResource oppijanumerorekisteriAsyncResource;
  private final TarjontaAsyncResource tarjontaAsyncResource;
  private final KoskiService koskiService;
  private final HakemuksetConverterUtil hakemuksetConverterUtil;
  private final OhjausparametritAsyncResource ohjausparametritAsyncResource;
  private final HakukohdeService hakukohdeService;

  private volatile int splittaus;

  @Autowired
  public LaskentaActorFactory(
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
    this.splittaus = splittaus;
    this.valintalaskentaResource = valintalaskentaResource;
    this.applicationAsyncResource = applicationAsyncResource;
    this.ataruAsyncResource = ataruAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
    this.seurantaDao = seurantaDao;
    this.suoritusrekisteriAsyncResource = suoritusrekisteriAsyncResource;
    this.tarjontaAsyncResource = tarjontaAsyncResource;
    this.valintapisteAsyncResource = valintapisteAsyncResource;
    this.koskiService = koskiService;
    this.hakemuksetConverterUtil = hakemuksetConverterUtil;
    this.oppijanumerorekisteriAsyncResource = oppijanumerorekisteriAsyncResource;
    this.ohjausparametritAsyncResource = ohjausparametritAsyncResource;
    this.hakukohdeService = hakukohdeService;
  }

  private LaskentaActor createValintaryhmaActor(LaskentaDto laskenta) {
    final Date nyt = new Date();
    AuditSession auditSession = koosteAuditSession(laskenta);
    Collection<HakukohdeJaOrganisaatio> hakukohteet = fetchHakukohteet(laskenta);

    LOG.info("Muodostetaan VALINTARYHMALASKENTA");
    auditLogLaskentaStart(auditSession, laskenta.getUuid(), laskenta.getHakuOid(), hakukohteet, "VALINTARYHMALASKENTA");

    return new LaskentaActorForSingleHakukohde(
        laskenta,
        Collections.singletonList(new HakukohdeJaOrganisaatio()),
        hakukohdeJaOrganisaatio -> {
          String uuid = laskenta.getUuid();
          Collection<String> hakukohdeOids =
              hakukohteet.stream()
                  .map(HakukohdeJaOrganisaatio::getHakukohdeOid)
                  .collect(Collectors.toList());
          String hakukohteidenNimi =
              String.format("Valintaryhmälaskenta %s hakukohteella", hakukohdeOids.size());
          LOG.info("(Uuid={}) {}", uuid, hakukohteidenNimi);

          CompletableFuture<String> tila = CompletableFuture.supplyAsync(() ->
            hakukohdeOids.stream().map(hakukohdeOid -> fetchResourcesForOneLaskenta(
                hakukohdeOid, auditSession, laskenta, true, true, nyt)
                .join()).toList())
              .thenApply(laskeDTOs -> {
                if (laskeDTOs.size() != hakukohdeOids.size()) {
                  throw new RuntimeException(
                      "Hakukohteita oli "
                          + hakukohdeOids.size()
                          + " mutta haettuja laskeDTOita oli "
                          + laskeDTOs.size()
                          + "!");
                }
                /*
                 * Tiksussa b15df0500 Merge remote-tracking branch
                 * 'origin/VTKU-181__valintaryhmalaskennan_kutsu_pienempiin_paloihin' tämän kutsun siirto
                 * valintalaskentakoostepalvelusta valintalaskentaan oli palasteltu moneen kutsuun. Kun kutsu ei
                 * enää mene verkon yli ei (käsittääkseni) ole enää mitää syytä palastella joten palatta takaisin
                 * yhteen kutsuun.
                 */
                return valintalaskentaResource.valintaryhmaLaskenta(laskenta.getUuid(), laskeDTOs);
              });
          return tila;
        }, seurantaDao, splittaus);
  }

  @ManagedOperation
  public void setLaskentaSplitCount(int splitCount) {
    this.splittaus = splitCount;
    LOG.info("Laskenta split count asetettu arvoon {}", splitCount);
  }

  private LaskentaActor createValintakoelaskentaActor(LaskentaDto laskenta) {
    final Date nyt = new Date();
    AuditSession auditSession = koosteAuditSession(laskenta);
    Collection<HakukohdeJaOrganisaatio> hakukohteet = fetchHakukohteet(laskenta);

    LOG.info("Muodostetaan VALINTAKOELASKENTA");
    auditLogLaskentaStart(auditSession, laskenta.getUuid(), laskenta.getHakuOid(), hakukohteet, "VALINTAKOELASKENTA");

    return new LaskentaActorForSingleHakukohde(
        laskenta,
        hakukohteet,
        hakukohdeJaOrganisaatio -> {
          String hakukohdeOid = hakukohdeJaOrganisaatio.getHakukohdeOid();

          CompletableFuture<String> tila = fetchResourcesForOneLaskenta(
              hakukohdeOid,
              auditSession,
              laskenta,
              false,
              false,
              nyt).thenApply(laskeDTO -> valintalaskentaResource.valintakoeLaskenta(laskeDTO));
          return tila;
        }, seurantaDao, splittaus);
  }

  private LaskentaActor createValintalaskentaActor(LaskentaDto laskenta) {
    AuditSession auditSession = koosteAuditSession(laskenta);
    Collection<HakukohdeJaOrganisaatio> hakukohteet = fetchHakukohteet(laskenta);

    LOG.info("Muodostetaan VALINTALASKENTA");
    auditLogLaskentaStart(auditSession, laskenta.getUuid(), laskenta.getHakuOid(), hakukohteet,"VALINTALASKENTA");

    final String uuid = laskenta.getUuid();
    final Date nyt = new Date();
    LOG.info(
        String.format(
            "Jos laskennassa %s on jonoja, joita ei lasketa %s jälkeen, ei haeta niille tietoja Koskesta.",
            laskenta.getUuid(), nyt));
    return new LaskentaActorForSingleHakukohde(
        laskenta,
        hakukohteet,
        hakukohdeJaOrganisaatio -> {
          String hakukohdeOid = hakukohdeJaOrganisaatio.getHakukohdeOid();
          LOG.info("(Uuid={}) Haetaan laskennan resursseja hakukohteelle {}", uuid, hakukohdeOid);

          CompletableFuture<String> tila = fetchResourcesForOneLaskenta(
              hakukohdeOid,
              auditSession,
              laskenta,
              false,
              true,
              nyt).thenApply(laskeDTO -> valintalaskentaResource.valintalaskenta(laskeDTO));
          return tila;
        }, seurantaDao, splittaus);
  }

  private LaskentaActor createValintalaskentaJaValintakoelaskentaActor(LaskentaDto laskenta) {
    final Date nyt = new Date();
    AuditSession auditSession = koosteAuditSession(laskenta);
    Collection<HakukohdeJaOrganisaatio> hakukohteet = fetchHakukohteet(laskenta);

    LOG.info(
        "Muodostetaan KAIKKI VAIHEET LASKENTA koska valinnanvaihe oli {} ja valintakoelaskenta ehto {}",
        laskenta.getValinnanvaihe(),
        laskenta.getValintakoelaskenta());
    auditLogLaskentaStart(auditSession, laskenta.getUuid(), laskenta.getHakuOid(), hakukohteet, "KAIKKI VAIHEET LASKENTA");

    final String uuid = laskenta.getUuid();
    return new LaskentaActorForSingleHakukohde(
        laskenta,
        hakukohteet,
        hakukohdeJaOrganisaatio -> {
          String hakukohdeOid = hakukohdeJaOrganisaatio.getHakukohdeOid();
          LOG.info(
              "(Uuid={}) Haetaan laskennan + valintakoelaskennan resursseja hakukohteelle {}",
              uuid,
              hakukohdeOid);

          CompletableFuture<String> tila = fetchResourcesForOneLaskenta(
              hakukohdeOid,
              auditSession,
              laskenta,
              false,
              true,
              nyt).thenApply(laskeDTO -> valintalaskentaResource.laskeKaikki(laskeDTO));
          return tila;
        }, seurantaDao, splittaus);
  }

  private static AuditSession koosteAuditSession(LaskentaDto laskenta) {
    final String userAgent = "-";
    final String inetAddress = "127.0.0.1";
    AuditSession auditSession =
        new AuditSession(laskenta.getUserOID(), Collections.emptyList(), userAgent, inetAddress);
    auditSession.setSessionId(laskenta.getUuid());
    auditSession.setPersonOid(laskenta.getUserOID());
    return auditSession;
  }

  private Collection<HakukohdeJaOrganisaatio> fetchHakukohteet(LaskentaDto laskenta) {
    Collection<HakukohdeJaOrganisaatio> hakukohteet;
    try {
      hakukohteet = hakukohdeService.fetchHakukohteet(laskenta);
    } catch (Throwable t) {
      throw new RuntimeException("Taustatietojen haku epäonnistui laskennalle " + laskenta.getUuid(), t);
    }

    if(hakukohteet.isEmpty()) {
      throw new RuntimeException("Haulla "
          + laskenta.getUuid()
          + " ei saatu hakukohteita! Onko valinnat synkronoitu tarjonnan kanssa?");
    }
    return hakukohteet;
  }

  public LaskentaActor createLaskentaActor(LaskentaDto laskenta) {
    LOG.info(
        String.format(
            "Jos laskennassa %s on jonoja, joita ei lasketa %s jälkeen, ei haeta niille tietoja Koskesta.",
            laskenta.getUuid(), new Date()));

    if (LaskentaTyyppi.VALINTARYHMALASKENTA.equals(laskenta.getTyyppi())) {
      return createValintaryhmaActor(laskenta);
    }
    if (LaskentaTyyppi.VALINTAKOELASKENTA.equals(laskenta.getTyyppi())) {
      return createValintakoelaskentaActor(laskenta);
    }
    if (LaskentaTyyppi.VALINTALASKENTA.equals(laskenta.getTyyppi())) {
      return createValintalaskentaActor(laskenta);
    }
    return createValintalaskentaJaValintakoelaskentaActor(laskenta);
  }

  private void auditLogLaskentaStart(AuditSession auditSession, String uuid, String hakuOid, Collection<HakukohdeJaOrganisaatio> hakukohteet, String tyyppi) {
    Map<String, String> additionalAuditInfo = new HashMap<>();
    additionalAuditInfo.put("tyyppi", tyyppi);
    additionalAuditInfo.put("uuid", uuid);
    additionalAuditInfo.put(
        "hakukohteet",
        hakukohteet.stream()
            .map(HakukohdeJaOrganisaatio::getHakukohdeOid)
            .collect(Collectors.toList())
            .toString());
    AuditLog.log(
        KoosteAudit.AUDIT,
        auditSession.asAuditUser(),
        ValintaperusteetOperation.LASKENTATOTEUTUS_LUONTI,
        ValintaResource.LASKENTATOTEUTUS,
        hakuOid,
        Changes.EMPTY,
        additionalAuditInfo);
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

  private CompletableFuture<LaskeDTO> fetchResourcesForOneLaskenta(
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

    PyynnonTunniste tunniste =
        new PyynnonTunniste(
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
      PyynnonTunniste tunniste,
      String resurssi,
      Supplier<CompletableFuture<T>> sourceFuture,
      boolean retry) {
    return LaskentaResurssinhakuWrapper.luoLaskentaResurssinHakuFuture(
        sourceFuture, tunniste.withNimi(resurssi), retry);
  }

  private <T> CompletableFuture<T> createResurssiFuture(
      PyynnonTunniste tunniste, String resurssi, Supplier<CompletableFuture<T>> sourceFuture) {
    return createResurssiFuture(tunniste, resurssi, sourceFuture, false);
  }
}
