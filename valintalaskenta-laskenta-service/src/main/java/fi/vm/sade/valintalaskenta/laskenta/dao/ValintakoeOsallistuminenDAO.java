package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;

public interface ValintakoeOsallistuminenDAO {
  ValintakoeOsallistuminen readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);

  void createOrUpdate(ValintakoeOsallistuminen v);

  boolean onkoEdeltavaValinnanvaiheOlemassa(
      String hakuOid, String hakukohdeOid, int jarjestysnumero);
}
