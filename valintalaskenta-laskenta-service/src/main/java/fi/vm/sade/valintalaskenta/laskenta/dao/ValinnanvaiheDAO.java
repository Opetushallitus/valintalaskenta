package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;

import java.util.List;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 12.01
 */
public interface ValinnanvaiheDAO {

    public Valinnanvaihe haeEdeltavaValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero);

    Valinnanvaihe haeViimeisinValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero);

    Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid);

    List<Valinnanvaihe> haeValinnanvaiheetJarjestysnumerolla(String hakuOid, String hakukohdeOid, int jarjestysnumero);

    void create(Valinnanvaihe valinnanvaihe);

    void poistaValinnanvaihe(Valinnanvaihe valinnanvaihe);
}
