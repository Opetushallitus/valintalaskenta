package fi.vm.sade.valintalaskenta.runner.service.impl;

import static fi.vm.sade.valintalaskenta.domain.dto.seuranta.IlmoitusDto.ilmoitus;

import fi.vm.sade.service.valintaperusteet.dto.HakukohdeViiteDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import fi.vm.sade.valintalaskenta.runner.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valintalaskenta.runner.dto.Maski;
import fi.vm.sade.valintalaskenta.runner.service.LuoLaskentaService;
import java.util.*;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class LuoLaskentaServiceImpl implements LuoLaskentaService {
  private static final Logger LOG = LoggerFactory.getLogger(LuoLaskentaServiceImpl.class);

  @Autowired private SeurantaDao seurantaDao;

  public LuoLaskentaServiceImpl() {}

  @Override
  public TunnisteDto kaynnistaLaskentaHaulle(
      String userOID,
      String haunNimi,
      String nimi,
      LaskentaTyyppi laskentatyyppi,
      boolean isValintakoelaskenta,
      Optional<Integer> valinnanvaihe,
      String hakuOid,
      Optional<Maski> maski,
      boolean isErillishaku,
      List<HakukohdeViiteDTO> hakukohdeViitteet) {

    LOG.info("Aloitetaan laskenta haulle {}", hakuOid);
    Collection<HakukohdeJaOrganisaatio> haunHakukohteetOids =
        kasitteleHakukohdeViitteet(hakukohdeViitteet, hakuOid, maski);

    final List<HakukohdeDto> hakukohdeDtos = toHakukohdeDto(haunHakukohteetOids);
    validateHakukohdeDtos(haunHakukohteetOids, hakukohdeDtos);
    TunnisteDto tunniste =
        seurantaDao.luoLaskenta(
            userOID,
            haunNimi,
            nimi,
            hakuOid,
            laskentatyyppi,
            isErillishaku,
            valinnanvaihe,
            isValintakoelaskenta,
            hakukohdeDtos);

    return tunniste;
  }

  @Override
  public TunnisteDto kaynnistaLaskentaUudelleen(final String uuid) {
    Optional<LaskentaDto> laskenta = this.haeLaskenta(uuid);
    if (laskenta.isPresent() && laskenta.get().getTila() != LaskentaTila.VALMIS) {
      return new TunnisteDto(uuid, false);
    }

    LaskentaDto laskentaDto = seurantaDao.resetoiLaskenta(uuid, true);
    if (laskentaDto == null) {
      LOG.error("Laskennan {} tila resetoitiin mutta ei saatu yhteenvetoa resetoinnista!", uuid);
    }

    return new TunnisteDto(laskentaDto.getUuid(), laskentaDto.getLuotiinkoUusiLaskenta());
  }

  @Override
  public void peruutaLaskenta(String uuid, Boolean lopetaVainJonossaOlevaLaskenta) {
    Optional<LaskentaDto> laskenta = this.haeLaskenta(uuid);
    if (!laskenta.isPresent()) {
      LOG.warn("Yritettiin peruuttaa olematonta laskentaa: " + uuid);
      return;
    }

    if (Boolean.TRUE.equals(lopetaVainJonossaOlevaLaskenta)) {
      boolean onkoLaskentaVielaJonossa = laskenta.get().getTila() == LaskentaTila.ALOITTAMATTA;
      if (!onkoLaskentaVielaJonossa) {
        LOG.warn(
            "Yritettiin peruuttaa laskentaa: "
                + uuid
                + " joka on tilassa "
                + laskenta.get().getTila());
        return;
      }
    }
    seurantaDao.peruutaLaskenta(uuid, Optional.of(ilmoitus("Peruutettu käyttäjän toimesta")));
  }

  @Override
  public Optional<LaskentaDto> haeLaskenta(String uuid) {
    return seurantaDao.haeLaskenta(uuid);
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
      Collection<HakukohdeJaOrganisaatio> hakukohdeData, List<HakukohdeDto> hakukohdeDtos) {
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
}