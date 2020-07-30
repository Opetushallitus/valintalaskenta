package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import java.util.List;

public interface ValintakoeOsallistuminenDAO {
  ValintakoeOsallistuminen readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);

  void createOrUpdate(ValintakoeOsallistuminen v);

  List<ValintakoeOsallistuminen> readAll();

  public ValintakoeOsallistuminen haeEdeltavaValinnanvaihe(
      String hakuOid, String hakukohdeOid, int jarjestysnumero);
}
