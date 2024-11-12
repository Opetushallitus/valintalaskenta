package fi.vm.sade.valinta.kooste.valintalaskenta.service;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoJarjestyskriteereillaDTO;
import fi.vm.sade.valinta.kooste.audit.AuditSession;
import fi.vm.sade.valinta.kooste.external.resource.koostepalvelu.KoostepalveluAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.audit.AuditLogUtil;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SuoritaLaskentaService {
  private static final Logger LOG = LoggerFactory.getLogger(SuoritaLaskentaService.class);

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

  private static AuditSession laskentaAuditSession(LaskentaDto laskenta) {
    final String userAgent = "-";
    final String inetAddress = "127.0.0.1";
    AuditSession auditSession =
        new AuditSession(laskenta.getUserOID(), Collections.emptyList(), userAgent, inetAddress);
    auditSession.setPersonOid(laskenta.getUserOID());
    return auditSession;
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
    AuditSession auditSession = laskentaAuditSession(laskenta);

    if (laskenta.getTyyppi().equals(fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi.VALINTARYHMA)) {
      String hakukohteidenNimi =
          String.format("Valintaryhmälaskenta %s hakukohteella", hakukohdeOids.size());
      LOG.info("Muodostetaan VALINTARYHMALASKENTA (Uuid={}) {}", laskenta.getUuid(), hakukohteidenNimi);
      AuditLogUtil.auditLogLaskenta(auditSession, ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS, laskenta.getUuid(),
          laskenta.getHakuOid(), hakukohdeOids, Optional.of("VALINTARYHMALASKENTA"));

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

    if(hakukohdeOids.size()!=1) {
      throw new RuntimeException("Hakukohteita on " + hakukohdeOids.size() + ". Muissa kuin valintaryhmälaskennassa hakukohteita täytyy olla tasan yksi!");
    }
    if(!this.isValintalaskentaKaytossa(laskenta, hakukohdeOids)) {
      return CompletableFuture.completedFuture(laskenta.getUuid());
    }

    if (laskenta.getValintakoelaskenta()) {
      String hakukohdeOid = hakukohdeOids.iterator().next();
      LOG.info("Muodostetaan VALINTAKOELASKENTA (Uuid={}) {}", laskenta.getUuid(), hakukohdeOid);
      AuditLogUtil.auditLogLaskenta(auditSession, ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS, laskenta.getUuid(),
          laskenta.getHakuOid(), hakukohdeOids, Optional.of("VALINTAKOELASKENTA"));
      return this.koostepalveluAsyncResource.haeLahtotiedot(
          laskenta,
          hakukohdeOid,
          false,
          false).thenApplyAsync(laskeDTO -> valintalaskentaResource.valintakoeLaskenta(laskeDTO), this.executor);
    } else {
      if (!laskenta.getValinnanvaihe().isPresent()) {
        String hakukohdeOid = hakukohdeOids.iterator().next();
        LOG.info("Muodostetaan KAIKKI VAIHEET LASKENTA (Uuid={}) {}", laskenta.getUuid(), hakukohdeOid);
        AuditLogUtil.auditLogLaskenta(auditSession, ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS, laskenta.getUuid(),
            laskenta.getHakuOid(), hakukohdeOids, Optional.of("KAIKKI VAIHEET LASKENTA"));

        Instant lahtotiedotStart = Instant.now();
        return this.koostepalveluAsyncResource.haeLahtotiedot(
            laskenta,
            hakukohdeOid,
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
        String hakukohdeOid = hakukohdeOids.iterator().next();
        LOG.info("Muodostetaan VALINTALASKENTA (Uuid={}) {}", laskenta.getUuid(), hakukohdeOid);
        AuditLogUtil.auditLogLaskenta(auditSession, ValintaperusteetOperation.LASKENTATOTEUTUS_KAYNNISTYS, laskenta.getUuid(),
            laskenta.getHakuOid(), hakukohdeOids, Optional.of("VALINTALASKENTA"));
        return this.koostepalveluAsyncResource.haeLahtotiedot(
            laskenta,
            hakukohdeOid,
            false,
            true).thenApplyAsync(laskeDTO -> valintalaskentaResource.valintalaskenta(laskeDTO), this.executor);
      }
    }
  }
}
