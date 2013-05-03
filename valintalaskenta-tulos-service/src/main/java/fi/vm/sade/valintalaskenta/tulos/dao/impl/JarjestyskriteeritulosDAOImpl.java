package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.code.morphia.Datastore;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.collection.VersioSet;
import fi.vm.sade.valintalaskenta.tulos.dao.JarjestyskriteeritulosDAO;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Repository
public class JarjestyskriteeritulosDAOImpl implements JarjestyskriteeritulosDAO {

    @Autowired
    private Datastore datastore;

    public List<Jarjestyskriteeritulos> readByValintatapajonoOid(String valintatapajonooid) {
        // valintatapajonoille on toteutettu java.lang.Comparable, joka
        // järjestää ne versioittain ensimmäinen versio ensimmäisenä ja
        // viimeisin versio viimeisenä(last)
        Valintatapajono uusinvalintatapajono = new VersioSet<Valintatapajono>(datastore.find(Valintatapajono.class,
                "valintatapajonooid", valintatapajonooid).asList()).haeUusinVersio();
        if (uusinvalintatapajono == null) {
            return Collections.<Jarjestyskriteeritulos> emptyList();
        }
        return uusinvalintatapajono.getJarjestyskriteeritulokset();
    }

}
