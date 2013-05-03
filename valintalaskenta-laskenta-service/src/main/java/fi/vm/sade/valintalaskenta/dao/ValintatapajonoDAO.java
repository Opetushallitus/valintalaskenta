package fi.vm.sade.valintalaskenta.dao;

import fi.vm.sade.valintalaskenta.domain.Valintatapajono;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public interface ValintatapajonoDAO {

    void createOrUpdate(Valintatapajono v);
}
