package fi.vm.sade.valintalaskenta.tulos.dao;

import java.util.List;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Jonosija;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public interface JarjestyskriteeritulosDAO {

    /**
     * 
     *
     * @param valintatapajonooid
     * @return
     */
    List<Jonosija> readByValintatapajonoOid(String valintatapajonooid);
}
