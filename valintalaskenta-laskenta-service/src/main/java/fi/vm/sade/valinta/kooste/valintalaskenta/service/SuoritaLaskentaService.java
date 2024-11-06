package fi.vm.sade.valinta.kooste.valintalaskenta.service;

import fi.vm.sade.auditlog.ApplicationType;
import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valinta.kooste.AuditSession;
import fi.vm.sade.valinta.kooste.external.resource.koostepalvelu.KoostepalveluAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.sharedutils.AuditLog;
import fi.vm.sade.valinta.sharedutils.AuditLogger;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SuoritaLaskentaService {
  private static final Logger LOG = LoggerFactory.getLogger(SuoritaLaskentaService.class);

  public static final Audit AUDIT =
      new Audit(new AuditLogger(), "valintalaskentakoostepalvelu", ApplicationType.VIRKAILIJA);

  private final ValintalaskentaResourceImpl valintalaskentaResource;
  private final KoostepalveluAsyncResource koostepalveluAsyncResource;
  private final ValintaperusteetAsyncResource valintaperusteetAsyncResource;
  private final ExecutorService executor = Executors.newWorkStealingPool();

  @Autowired
  public SuoritaLaskentaService(
      ValintalaskentaResourceImpl valintalaskentaResource,
      KoostepalveluAsyncResource koostepalveluAsyncResource,
      ValintaperusteetAsyncResource valintaperusteetAsyncResource) {
    this.valintalaskentaResource = valintalaskentaResource;
    this.koostepalveluAsyncResource = koostepalveluAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
  }

  private boolean isValintalaskentaKaytossa(LaskentaDto laskenta, Collection<String> hakukohdeOids) {
    List<ValintaperusteetDTO> valintaperusteet = valintaperusteetAsyncResource.haeValintaperusteet(hakukohdeOids.iterator().next(), laskenta.getValinnanvaihe()).join();
    boolean jokinValintatapajonoKayttaaValintalaskentaa =
        valintaperusteet.stream()
            .map(ValintaperusteetDTO::getValinnanVaihe)
            .flatMap(v -> v.getValintatapajono().stream())
            .anyMatch(ValintatapajonoJarjestyskriteereillaDTO::getKaytetaanValintalaskentaa);

    return jokinValintatapajonoKayttaaValintalaskentaa;
  }

  public CompletableFuture<String> suoritaLaskentaHakukohteille(LaskentaDto laskenta, Collection<String> hakukohdeOids) {
    if (laskenta.getTyyppi().equals(fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi.VALINTARYHMA)) {
      String hakukohteidenNimi =
          String.format("Valintaryhmälaskenta %s hakukohteella", hakukohdeOids.size());
      LOG.info("(Uuid={}) {}", laskenta.getUuid(), hakukohteidenNimi);

      return CompletableFuture.supplyAsync(() ->
              hakukohdeOids.stream().map(hakukohdeOid -> this.koostepalveluAsyncResource.haeLahtotiedot(
                      laskenta, hakukohdeOid, true, true)
                  .join()).toList())
          .thenApplyAsync(laskeDTOs -> {
            /*
             * Tiksussa b15df0500 Merge remote-tracking branch
             * 'origin/VTKU-181__valintaryhmalaskennan_kutsu_pienempiin_paloihin' tämän kutsun siirto
             * valintalaskentakoostepalvelusta valintalaskentaan oli palasteltu moneen kutsuun. Kun kutsu ei
             * enää mene verkon yli ei (käsittääkseni) ole enää mitää syytä palastella joten palatta takaisin
             * yhteen kutsuun.
             */
            return valintalaskentaResource.valintaryhmaLaskenta(laskenta.getUuid(), laskeDTOs);
          }, this.executor);
    }

    if(!this.isValintalaskentaKaytossa(laskenta, hakukohdeOids)) {
      return CompletableFuture.completedFuture(laskenta.getUuid());
    }

    if (Boolean.TRUE.equals(laskenta.getValintakoelaskenta())) {
      LOG.info("Muodostetaan VALINTAKOELASKENTA");
      return this.koostepalveluAsyncResource.haeLahtotiedot(
          laskenta,
          hakukohdeOids.iterator().next(),
          false,
          false).thenApplyAsync(laskeDTO -> valintalaskentaResource.valintakoeLaskenta(laskeDTO), this.executor);
    } else {
      if (laskenta.getValinnanvaihe() == null || laskenta.getValinnanvaihe() == -1) {
        LOG.info(
            "(Uuid={}) Haetaan laskennan + valintakoelaskennan resursseja hakukohteelle {}",
            laskenta.getUuid(),
            hakukohdeOids.iterator().next());

        Instant lahtotiedotStart = Instant.now();
        return this.koostepalveluAsyncResource.haeLahtotiedot(
            laskenta,
            hakukohdeOids.iterator().next(),
            false,
            true).thenApplyAsync(laskeDTO -> {
          Duration lahtotiedotDuration = Duration.between(lahtotiedotStart, Instant.now());
          Instant laskeStart = Instant.now();
          String result = valintalaskentaResource.laskeKaikki(laskeDTO);
          Duration laskeDuration = Duration.between(laskeStart, Instant.now());

          LOG.info("Kesto: Hakukohde: {}, lähtotiedot: {} ms, laskenta: {} ms", hakukohdeOids.iterator().next(), lahtotiedotDuration.toMillis(), laskeDuration.toMillis());

          return result;
        }, this.executor);
      } else {
        LOG.info("(Uuid={}) Haetaan laskennan resursseja hakukohteelle {}", laskenta.getUuid(), hakukohdeOids.iterator().next());
        return this.koostepalveluAsyncResource.haeLahtotiedot(
            laskenta,
            hakukohdeOids.iterator().next(),
            false,
            true).thenApplyAsync(laskeDTO -> valintalaskentaResource.valintalaskenta(laskeDTO), this.executor);
      }
    }
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
        AUDIT,
        auditSession.asAuditUser(),
        ValintaperusteetOperation.LASKENTATOTEUTUS_LUONTI,
        ValintaResource.LASKENTATOTEUTUS,
        hakuOid,
        Changes.EMPTY,
        additionalAuditInfo);
  }
}
