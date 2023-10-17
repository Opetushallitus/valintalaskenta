package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValintakoeOsallistuminenRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

@Repository("ValintakoeOsallistuminenDAO")
public class ValintakoeOsallistuminenDAOImpl implements ValintakoeOsallistuminenDAO {

  @Value("${valintalaskenta-laskenta-service.mongodb.useIndexQueries:false}")
  private boolean useIndexQueries;

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
  public ValintakoeOsallistuminen haeEdeltavaValinnanvaihe(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    if (jarjestysnumero == 0) return null;
    return repo.findByHakuHakukohdeAndValinnanvaiheJarjestysLuku(
            hakuOid, hakukohdeOid, jarjestysnumero - 1)
        .orElse(null);
  }
}
