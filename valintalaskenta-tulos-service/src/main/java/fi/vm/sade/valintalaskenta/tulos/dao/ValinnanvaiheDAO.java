package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;

import java.util.List;

public interface ValinnanvaiheDAO {
    List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid);

    List<Valinnanvaihe> readByHakuOid(String hakuoid);

    List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid);

    Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid);

    Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid);

    void saveOrUpdate(Valinnanvaihe vaihe);
}
