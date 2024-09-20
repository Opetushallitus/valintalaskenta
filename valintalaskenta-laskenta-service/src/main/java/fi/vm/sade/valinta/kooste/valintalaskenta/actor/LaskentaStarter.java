package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import akka.actor.ActorRef;
import fi.vm.sade.service.valintaperusteet.dto.HakukohdeViiteDTO;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.OhjausparametritAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.dto.ParametritDTO;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.Haku;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.TarjontaAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.AuditSession;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Maski;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class LaskentaStarter {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaStarter.class);

  private final OhjausparametritAsyncResource ohjausparametritAsyncResource;
  private final ValintaperusteetAsyncResource valintaperusteetAsyncResource;
  private final TarjontaAsyncResource tarjontaAsyncResource;
  private final SeurantaDao seurantaDao;

  @Autowired
  public LaskentaStarter(
      OhjausparametritAsyncResource ohjausparametritAsyncResource,
      ValintaperusteetAsyncResource valintaperusteetAsyncResource,
      TarjontaAsyncResource tarjontaAsyncResource,
      SeurantaDao seurantaDao) {
    this.ohjausparametritAsyncResource = ohjausparametritAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
    this.tarjontaAsyncResource = tarjontaAsyncResource;
    this.seurantaDao = seurantaDao;
  }

  public void fetchLaskentaParams(
      ActorRef laskennanKaynnistajaActor,
      final String uuid,
      final BiConsumer<Haku, LaskentaActorParams> startActor) {

    LaskentaDto laskenta = seurantaDao.haeLaskenta(uuid).get();
    String hakuOid = laskenta.getHakuOid();
    if (StringUtils.isBlank(hakuOid)) {
      LOG.error("Yritettiin hakea hakukohteita ilman hakuOidia!");
      throw new RuntimeException("Yritettiin hakea hakukohteita ilman hakuOidia!");
    }

    CompletableFuture<Haku> haku = tarjontaAsyncResource.haeHaku(hakuOid);
    CompletableFuture<List<HakukohdeViiteDTO>> hakukohteet = valintaperusteetAsyncResource.haunHakukohteet(hakuOid);
    CompletableFuture<ParametritDTO> parametrit = ohjausparametritAsyncResource.haeHaunOhjausparametrit(hakuOid);
    CompletableFuture.allOf(haku, hakukohteet, parametrit).join();

    try {
      Collection<HakukohdeJaOrganisaatio> hakukohdeOids = maskHakukohteet(hakuOid, hakukohteet.get(), laskenta);
      if (!hakukohdeOids.isEmpty()) {
        startActor.accept(haku.get(), laskentaActorParams(hakuOid, laskenta, hakukohdeOids, parametrit.get()));
      } else {
        cancelLaskenta(
            laskennanKaynnistajaActor,
            "Haulla "
                + laskenta.getUuid()
                + " ei saatu hakukohteita! Onko valinnat synkronoitu tarjonnan kanssa?",
            null,
            uuid);
      }
    } catch(Throwable t) {
      cancelLaskenta(
          laskennanKaynnistajaActor,
          "Taustatietojen haku epäonnistui haku epäonnistui",
          Optional.of(t),
          laskenta.getUuid());
    }
  }

  private static Collection<HakukohdeJaOrganisaatio> maskHakukohteet(
      String hakuOid, List<HakukohdeViiteDTO> hakukohdeViitteet, LaskentaDto laskenta) {
    LOG.info("Tarkastellaan hakukohdeviitteita haulle {}", hakuOid);

    final List<HakukohdeJaOrganisaatio> haunHakukohdeOidit =
        hakukohdeViitteet != null
            ? publishedNonNulltoHakukohdeJaOrganisaatio(hakukohdeViitteet)
            : new ArrayList<>();
    final Maski maski = createMaskiFromLaskenta(laskenta);

    return maski.maskaa(haunHakukohdeOidit);
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

  private static LaskentaActorParams laskentaActorParams(
      String hakuOid,
      LaskentaDto laskenta,
      Collection<HakukohdeJaOrganisaatio> haunHakukohdeOidit,
      ParametritDTO parametrit) {
    return new LaskentaActorParams(
        new LaskentaStartParams(
            koosteAuditSession(laskenta),
            laskenta.getUuid(),
            hakuOid,
            laskenta.isErillishaku(),
            true,
            LaskentaTyyppi.VALINTARYHMA.equals(laskenta.getTyyppi()),
            laskenta.getValinnanvaihe(),
            laskenta.getValintakoelaskenta(),
            haunHakukohdeOidit,
            laskenta.getTyyppi()),
        parametrit);
  }

  private static List<HakukohdeJaOrganisaatio> publishedNonNulltoHakukohdeJaOrganisaatio(
      final List<HakukohdeViiteDTO> hakukohdeViitteet) {
    return hakukohdeViitteet.stream()
        .filter(Objects::nonNull)
        .filter(h -> h.getOid() != null)
        .filter(h -> h.getTila().equals("JULKAISTU"))
        .map(h -> new HakukohdeJaOrganisaatio(h.getOid(), h.getTarjoajaOid()))
        .collect(Collectors.toList());
  }

  private void cancelLaskenta(
      ActorRef laskennanKaynnistajaActor, String msg, Optional<Throwable> t, String uuid) {
    if (t.isPresent()) LOG.error(msg, t);
    else LOG.error(msg);
    LaskentaTila tila = LaskentaTila.VALMIS;
    HakukohdeTila hakukohdetila = HakukohdeTila.KESKEYTETTY;
    Optional<IlmoitusDto> ilmoitusDtoOptional =
        t.map(
            poikkeus -> IlmoitusDto.virheilmoitus(msg, Arrays.toString(poikkeus.getStackTrace())));

    seurantaDao.merkkaaTila(uuid, tila, hakukohdetila, ilmoitusDtoOptional);

    // TODO: tämä toimii käsittääkseni oikein vain jos laskenta oli käynnissä
    laskennanKaynnistajaActor.tell(new LaskentaStarterActor.WorkerAvailable(), ActorRef.noSender());
  }

  private static Maski createMaskiFromLaskenta(final LaskentaDto laskenta) {
    final List<String> hakukohdeOids =
        laskenta.getHakukohteet().stream()
            .filter(h -> !HakukohdeTila.VALMIS.equals(h.getTila()))
            .map(h -> new HakukohdeJaOrganisaatio(h.getHakukohdeOid(), h.getOrganisaatioOid()))
            .map(HakukohdeJaOrganisaatio::getHakukohdeOid)
            .collect(Collectors.toList());

    return Maski.whitelist(hakukohdeOids);
  }
}
