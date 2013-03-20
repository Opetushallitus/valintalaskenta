package fi.vm.sade.valintalaskenta.dao;

import java.util.List;

import fi.vm.sade.valintalaskenta.domain.Valintatapajono;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public interface ValintatapajonoDAO {

    List<Valintatapajono> readByValinnanvaiheOid(String valinnanvaiheOid);

}
