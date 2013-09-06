package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;

import java.util.List;

/**
 * @author Jussi Jartamo
 */
public interface ValinnanvaiheDAO {

    List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid);

    List<Valinnanvaihe> readByHakuOid(String hakuoid);

    List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);

    Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid);
}
