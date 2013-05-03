package fi.vm.sade.valintalaskenta.tulos.dao;

import java.util.List;

import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public interface ValinnanvaiheDAO {

    List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid);
}
