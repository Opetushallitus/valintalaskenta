package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValintakoeOsallistuminenRepository;
import org.springframework.stereotype.Service;

@Service
public class ValintakoeOsallistuminenDAOImpl implements ValintakoeOsallistuminenDAO {

  private final ValintakoeOsallistuminenRepository repo;

  public ValintakoeOsallistuminenDAOImpl(ValintakoeOsallistuminenRepository repo) {
    this.repo = repo;
  }

  @Override
  public ValintakoeOsallistuminen readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    return repo.findByHakemusOidAndHakuOid(hakemusOid, hakuOid).orElse(null);
  }

  @Override
  public void createOrUpdate(ValintakoeOsallistuminen v) {
    repo.save(v);
  }

  @Override
  public boolean onkoEdeltavaValinnanvaiheOlemassa(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    if (jarjestysnumero == 0) return false;
    return repo.findByHakuHakukohdeAndValinnanvaiheJarjestysLuku(
            hakuOid, hakukohdeOid, jarjestysnumero - 1)
        .isPresent();
  }
}
