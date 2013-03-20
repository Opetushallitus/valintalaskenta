package fi.vm.sade.valintalaskenta.dao;

import java.util.List;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public interface JarjestyskriteeritulosDAO {

    /**
     * 
     * @param valintatapajonooid
     * @return
     */
    List<Jarjestyskriteeritulos> readByValintatapajonoOid(String valintatapajonooid);
}
