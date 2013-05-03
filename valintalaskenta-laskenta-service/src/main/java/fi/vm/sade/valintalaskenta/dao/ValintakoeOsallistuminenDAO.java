package fi.vm.sade.valintalaskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 14.04
 */
public interface ValintakoeOsallistuminenDAO {
    ValintakoeOsallistuminen readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);

    void createOrUpdate(ValintakoeOsallistuminen v);
}
