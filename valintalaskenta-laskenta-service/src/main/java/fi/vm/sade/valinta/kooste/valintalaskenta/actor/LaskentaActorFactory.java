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
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Service;

/*
TODO: Varmista että tämän luokan metodeja kutsuttaessa edelleen pätee  @PreAuthorize(OPH_CRUD)
 */
@Service
public class LaskentaActorFactory {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaActorFactory.class);

  private final LaskentaResurssiProvider laskentaResurssiProvider;
  private final ValintalaskentaResourceImpl valintalaskentaResource;
  private final ExecutorService executor = Executors.newWorkStealingPool();


  @Autowired
  public LaskentaActorFactory(
      LaskentaResurssiProvider laskentaResurssiProvider,
      ValintalaskentaResourceImpl valintalaskentaResource) {
    this.laskentaResurssiProvider = laskentaResurssiProvider;
    this.valintalaskentaResource = valintalaskentaResource;
  }

  public CompletableFuture<String> suoritaLaskentaHakukohteille(LaskentaDto laskenta, Collection<String> hakukohdeOids) {
    if (laskenta.getTyyppi().equals(fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi.VALINTARYHMA)) {
      String hakukohteidenNimi =
          String.format("Valintaryhmälaskenta %s hakukohteella", hakukohdeOids.size());
      LOG.info("(Uuid={}) {}", laskenta.getUuid(), hakukohteidenNimi);

      return CompletableFuture.supplyAsync(() ->
              hakukohdeOids.stream().map(hakukohdeOid -> this.laskentaResurssiProvider.fetchResourcesForOneLaskenta(
                  hakukohdeOid, koosteAuditSession(laskenta), laskenta, true, true, new Date())
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
    if (Boolean.TRUE.equals(laskenta.getValintakoelaskenta())) {
      LOG.info("Muodostetaan VALINTAKOELASKENTA");
      return this.laskentaResurssiProvider.fetchResourcesForOneLaskenta(
          hakukohdeOids.iterator().next(),
          koosteAuditSession(laskenta),
          laskenta,
          false,
          false,
          new Date()).thenApplyAsync(laskeDTO -> valintalaskentaResource.valintakoeLaskenta(laskeDTO), this.executor);
    } else {
      if (laskenta.getValinnanvaihe() == null) {
        LOG.info(
            "(Uuid={}) Haetaan laskennan + valintakoelaskennan resursseja hakukohteelle {}",
            laskenta.getUuid(),
            hakukohdeOids.iterator().next());

        return this.laskentaResurssiProvider.fetchResourcesForOneLaskenta(
            hakukohdeOids.iterator().next(),
            koosteAuditSession(laskenta),
            laskenta,
            false,
            true,
            new Date()).thenApplyAsync(laskeDTO -> valintalaskentaResource.laskeKaikki(laskeDTO), this.executor);
      } else {
        LOG.info("(Uuid={}) Haetaan laskennan resursseja hakukohteelle {}", laskenta.getUuid(), hakukohdeOids.iterator().next());
        return this.laskentaResurssiProvider.fetchResourcesForOneLaskenta(
            hakukohdeOids.iterator().next(),
            koosteAuditSession(laskenta),
            laskenta,
            false,
            true,
            new Date()).thenApplyAsync(laskeDTO -> valintalaskentaResource.valintalaskenta(laskeDTO), this.executor);
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
        KoosteAudit.AUDIT,
        auditSession.asAuditUser(),
        ValintaperusteetOperation.LASKENTATOTEUTUS_LUONTI,
        ValintaResource.LASKENTATOTEUTUS,
        hakuOid,
        Changes.EMPTY,
        additionalAuditInfo);
  }
}
