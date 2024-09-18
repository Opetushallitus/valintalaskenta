package fi.vm.sade.valinta.kooste.seuranta.impl;

import fi.vm.sade.valinta.kooste.seuranta.LaskentaSeurantaService;
import fi.vm.sade.valinta.kooste.valintalaskenta.resource.LaskentaParams;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import io.reactivex.Observable;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class LaskentaSeurantaServiceImpl implements LaskentaSeurantaService {
  private final Logger LOG = LoggerFactory.getLogger(getClass());

  private final SeurantaDao seurantaDao;

  @Autowired
  public LaskentaSeurantaServiceImpl(SeurantaDao seurantaDao) {
    this.seurantaDao = seurantaDao;
  }

  @Override
  public Optional<String> otaSeuraavaLaskentaTyonAlle() {
    return Optional.ofNullable(seurantaDao.otaSeuraavaLaskentaTyonAlle());
  }

  public Observable<LaskentaDto> laskenta(String uuid) {
    LaskentaDto l = seurantaDao.haeLaskenta(uuid);
    if (l == null) {
      throw new RuntimeException("SeurantaDao palautti null olion uuid:lle " + uuid);
    }
    return Observable.fromFuture(CompletableFuture.completedFuture(l));
  }

  public Observable<LaskentaDto> resetoiTilat(String uuid) {
    LaskentaDto ldto = seurantaDao.resetoiEiValmiitHakukohteet(uuid, true);
    if (ldto == null) {
      LOG.error("Laskennan {} tila resetoitiin mutta ei saatu yhteenvetoa resetoinnista!", uuid);
    }
    return Observable.fromFuture(CompletableFuture.completedFuture(ldto));
  }

  public TunnisteDto luoLaskenta(
      LaskentaParams laskentaParams, List<HakukohdeDto> hakukohdeOids) {
    if (hakukohdeOids == null) {
      throw new NullPointerException(
          "Laskentaa ei luoda tyhjalle (null) hakukohdedto referenssille!");
    }
    if (hakukohdeOids.isEmpty()) {
      throw new NullPointerException(
          "Laskentaa ei luoda tyhjalle (koko on nolla) hakukohdedto joukolle!");
    }
    hakukohdeOids.forEach(
        hk -> {
          if (hk.getHakukohdeOid() == null || hk.getOrganisaatioOid() == null) {
            throw new NullPointerException(
                "Laskentaa ei luoda hakukohdejoukkoobjektille koska joukossa oli null referensseja sisaltava hakukohde!");
          }
        });
    return seurantaDao.luoLaskenta(
        laskentaParams.getUserOID(),
        laskentaParams.getHaunNimi(),
        laskentaParams.getNimi(),
        laskentaParams.getHakuOid(),
        laskentaParams.getLaskentatyyppi(),
        laskentaParams.isErillishaku(),
        laskentaParams.getValinnanvaihe(),
        laskentaParams.getIsValintakoelaskenta(),
        hakukohdeOids);
  }

  public Observable<YhteenvetoDto> merkkaaLaskennanTila(
      String uuid, LaskentaTila tila, Optional<IlmoitusDto> ilmoitusDtoOptional) {
    YhteenvetoDto y = seurantaDao.merkkaaTila(uuid, tila, ilmoitusDtoOptional);
    if (y == null) {
      LOG.error(
          "Seurantaan paivitettiin laskennan {} tila {} mutta ei saatu yhteenvetoa lisayksesta!",
          uuid,
          tila);
    }
    return Observable.fromFuture(CompletableFuture.completedFuture(y));
  }

  public Observable<YhteenvetoDto> merkkaaLaskennanTila(
      String uuid,
      LaskentaTila tila,
      HakukohdeTila hakukohdetila,
      Optional<IlmoitusDto> ilmoitusDtoOptional) {

    YhteenvetoDto y =
        seurantaDao.merkkaaTila(uuid, tila, hakukohdetila, ilmoitusDtoOptional);
    if (y == null) {
      LOG.error(
          "Seurantaan paivitettiin laskennan {} tila {} mutta ei saatu yhteenvetoa lisayksesta!",
          uuid,
          tila);
    }
    return Observable.fromFuture(CompletableFuture.completedFuture(y));
  }

  @Override
  public Observable<YhteenvetoDto> merkkaaHakukohteenTila(
      String uuid,
      String hakukohdeOid,
      HakukohdeTila tila,
      Optional<IlmoitusDto> ilmoitusDtoOptional) {

    YhteenvetoDto y;
    if(ilmoitusDtoOptional.isPresent()) {
      y = seurantaDao.merkkaaTila(uuid, hakukohdeOid, tila, ilmoitusDtoOptional.get());
    } else {
      y = seurantaDao.merkkaaTila(uuid, hakukohdeOid, tila);
    }
    if (y == null) {
      LOG.error(
          "Seurantaan markattiin hakukohteen {} tila {} laskentaan {} mutta ei saatu yhteenvetoa lisayksesta!",
          hakukohdeOid,
          tila,
          uuid);
    }
    return Observable.fromFuture(CompletableFuture.completedFuture(y));
  }
}
