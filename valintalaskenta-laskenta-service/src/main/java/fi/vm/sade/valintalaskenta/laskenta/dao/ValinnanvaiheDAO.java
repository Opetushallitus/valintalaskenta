package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 12.01
 */
public interface ValinnanvaiheDAO {

    public Valinnanvaihe haeEdellinenValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero);

    Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid);

    void create(Valinnanvaihe valinnanvaihe);
}
