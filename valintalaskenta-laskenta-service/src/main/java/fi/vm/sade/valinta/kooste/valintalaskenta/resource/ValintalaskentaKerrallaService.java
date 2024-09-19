package fi.vm.sade.valinta.kooste.valintalaskenta.resource;

import fi.vm.sade.service.valintaperusteet.dto.HakukohdeViiteDTO;
import fi.vm.sade.valinta.kooste.valintalaskenta.actor.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Laskenta;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaInfo;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Maski;
import fi.vm.sade.valinta.kooste.valintalaskenta.route.ValintalaskentaKerrallaRoute;
import fi.vm.sade.valinta.kooste.valintalaskenta.route.ValintalaskentaKerrallaRouteValvomo;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.HakukohdeDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTila;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.TunnisteDto;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import io.reactivex.Observable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import static fi.vm.sade.valintalaskenta.domain.dto.seuranta.IlmoitusDto.ilmoitus;

@Service
public class ValintalaskentaKerrallaService {
  private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaKerrallaService.class);

  @Autowired private ValintalaskentaKerrallaRouteValvomo valintalaskentaValvomo;
  @Autowired private ValintalaskentaKerrallaRoute valintalaskentaRoute;
  @Autowired private SeurantaDao seurantaDao;

  public ValintalaskentaKerrallaService() {}

  public TunnisteDto kaynnistaLaskentaHaulle(LaskentaParams laskentaParams,
                                             List<HakukohdeViiteDTO> hakukohdeViitteet) {
    String hakuOid = laskentaParams.getHakuOid();
    Optional<String> uuidForExistingNonMaskedLaskenta =
        uuidForExistingNonMaskedLaskenta(laskentaParams.getMaski(), hakuOid);

    if (uuidForExistingNonMaskedLaskenta.isPresent()) {
      String uuid = uuidForExistingNonMaskedLaskenta.get();
      LOG.warn(
          "Laskenta on jo kaynnissa haulle {} joten palautetaan seurantatunnus({}) ajossa olevaan hakuun",
          uuid,
          uuid);
      return new TunnisteDto(uuid, false);
    }

    LOG.info("Aloitetaan laskenta haulle {}", hakuOid);
    Collection<HakukohdeJaOrganisaatio> haunHakukohteetOids =
        kasitteleHakukohdeViitteet(
            hakukohdeViitteet, hakuOid, laskentaParams.getMaski());

    final List<HakukohdeDto> hakukohdeDtos = toHakukohdeDto(haunHakukohteetOids);
    validateHakukohdeDtos(haunHakukohteetOids, hakukohdeDtos);
    TunnisteDto tunniste = seurantaDao.luoLaskenta(
        laskentaParams.getUserOID(),
        laskentaParams.getHaunNimi(),
        laskentaParams.getNimi(),
        laskentaParams.getHakuOid(),
        laskentaParams.getLaskentatyyppi(),
        laskentaParams.isErillishaku(),
        laskentaParams.getValinnanvaihe(),
        laskentaParams.getIsValintakoelaskenta(),
        hakukohdeDtos);
    if (tunniste.getLuotiinkoUusiLaskenta()) {
      valintalaskentaRoute.workAvailable();
    }
    return tunniste;
  }

  public TunnisteDto kaynnistaLaskentaUudelleen(final String uuid) {
    Optional<Laskenta> laskenta = valintalaskentaValvomo.fetchLaskenta(uuid);
    if(laskenta.isPresent() && !laskenta.get().isValmis()) {
      return new TunnisteDto(uuid, false);
    }

    LaskentaDto laskentaDto = seurantaDao.resetoiEiValmiitHakukohteet(uuid, true);
    if (laskentaDto == null) {
      LOG.error("Laskennan {} tila resetoitiin mutta ei saatu yhteenvetoa resetoinnista!", uuid);
    }

    if (laskentaDto.getLuotiinkoUusiLaskenta()) {
      valintalaskentaRoute.workAvailable();
    }
    return new TunnisteDto(laskentaDto.getUuid(), laskentaDto.getLuotiinkoUusiLaskenta());
  }

  public void peruutaLaskenta(String uuid, Boolean lopetaVainJonossaOlevaLaskenta) {
    if (Boolean.TRUE.equals(lopetaVainJonossaOlevaLaskenta)) {
      boolean onkoLaskentaVielaJonossa = valintalaskentaValvomo.fetchLaskenta(uuid) == null;
      if (!onkoLaskentaVielaJonossa) {
        // Laskentaa suoritetaan jo joten ei pysayteta
        return;
      }
    }
    stop(uuid);
    Observable.fromFuture(CompletableFuture.completedFuture(seurantaDao
        .merkkaaTila(
            uuid, LaskentaTila.PERUUTETTU, Optional.of(ilmoitus("Peruutettu käyttäjän toimesta")))))
        .subscribe(ok -> stop(uuid), nok -> stop(uuid));
  }

  private void stop(String uuid) {
    valintalaskentaValvomo.fetchLaskenta(uuid).ifPresent(Laskenta::lopeta);
  }

  private Optional<Laskenta> haeAjossaOlevaLaskentaHaulle(final String hakuOid) {
    return valintalaskentaValvomo.runningLaskentas().stream()
        .filter(l -> hakuOid.equals(l.getHakuOid()) && !l.isOsittainenLaskenta())
        .findFirst();
  }

  public Optional<LaskentaDto> haeLaskenta(String uuid) {
    return Optional.ofNullable(seurantaDao.haeLaskenta(uuid).get());
  }

  private static Collection<HakukohdeJaOrganisaatio> kasitteleHakukohdeViitteet(
      final List<HakukohdeViiteDTO> hakukohdeViitteet,
      final String hakuOid,
      final Optional<Maski> maski) {
    LOG.info("Tarkastellaan hakukohdeviitteita haulle {}", hakuOid);

    if (hakukohdeViitteet == null || hakukohdeViitteet.isEmpty()) {
      LOG.error("Valintaperusteet palautti tyhjat hakukohdeviitteet haulle {}!", hakuOid);
      throw new NullPointerException("Valintaperusteet palautti tyhjat hakukohdeviitteet!");
    }
    final List<HakukohdeJaOrganisaatio> haunHakukohdeOids =
        hakukohdeViitteet.stream()
            .filter(Objects::nonNull)
            .filter(hakukohdeOid -> hakukohdeOid.getOid() != null)
            .filter(hakukohdeOid -> hakukohdeOid.getTila().equals("JULKAISTU"))
            .map(u -> new HakukohdeJaOrganisaatio(u.getOid(), u.getTarjoajaOid()))
            .collect(Collectors.toList());

    Collection<HakukohdeJaOrganisaatio> oids =
        maski.map(m -> m.maskaa(haunHakukohdeOids)).orElse(haunHakukohdeOids);
    if (oids.isEmpty()) {
      String msg =
          "Haulla "
              + hakuOid
              + " ei saatu hakukohteita! Onko valinnat synkronoitu tarjonnan kanssa?";
      LOG.error(msg);
      throw new RuntimeException(msg);
    } else {
      return oids;
    }
  }

  private static void validateHakukohdeDtos(
      Collection<HakukohdeJaOrganisaatio> hakukohdeData,
      List<HakukohdeDto> hakukohdeDtos) {
    if (hakukohdeDtos == null) {
      throw new NullPointerException(
          "Laskentaa ei luoda tyhjalle (null) hakukohdedto referenssille!");
    }
    if (hakukohdeDtos.isEmpty()) {
      String msg = "Laskentaa ei voida aloittaa hakukohteille joilta puuttuu organisaatio!";
      LOG.error(msg);
      throw new RuntimeException(
          "Laskentaa ei luoda tyhjalle (koko on nolla) hakukohdedto joukolle!");
    }
    hakukohdeDtos.forEach(
        hk -> {
          if (hk.getHakukohdeOid() == null || hk.getOrganisaatioOid() == null) {
            throw new NullPointerException(
                "Laskentaa ei luoda hakukohdejoukkoobjektille koska joukossa oli null referensseja sisaltava hakukohde!");
          }
        });
    if (hakukohdeDtos.size() < hakukohdeData.size()) {
      LOG.warn(
          "Hakukohteita puuttuvien organisaatio-oidien vuoksi filtteroinnin jalkeen {}/{}!",
          hakukohdeDtos.size(),
          hakukohdeData.size());
    } else {
      LOG.info(
          "Hakukohteita filtteroinnin jalkeen {}/{}!", hakukohdeDtos.size(), hakukohdeData.size());
    }
  }

  private static List<HakukohdeDto> toHakukohdeDto(
      Collection<HakukohdeJaOrganisaatio> hakukohdeData) {
    return hakukohdeData.stream()
        .filter(Objects::nonNull)
        .filter(hk -> hk.getHakukohdeOid() != null)
        .filter(hk -> hk.getOrganisaatioOid() != null)
        .map(hk -> new HakukohdeDto(hk.getHakukohdeOid(), hk.getOrganisaatioOid()))
        .collect(Collectors.toList());
  }

  private Optional<String> uuidForExistingNonMaskedLaskenta(Optional<Maski> maski, String hakuOid) {
    final Optional<Laskenta> ajossaOlevaLaskentaHaulle =
        !maski.isPresent() || !maski.get().isMask()
            ? haeAjossaOlevaLaskentaHaulle(hakuOid)
            : Optional.empty();
    return ajossaOlevaLaskentaHaulle.map(LaskentaInfo::getUuid);
  }
}
