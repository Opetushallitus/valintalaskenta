package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.tulos.dao.JarjestyskriteeritulosDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Repository
public class JarjestyskriteeritulosDAOImpl implements JarjestyskriteeritulosDAO {

    @Autowired
    private Datastore datastore;

    /*
    public List<Jonosija> readByValintatapajonoOid(String valintatapajonooid) {
        // valintatapajonoille on toteutettu java.lang.Comparable, joka
        // järjestää ne versioittain ensimmäinen versio ensimmäisenä ja
        // viimeisin versio viimeisenä(last)
        Valintatapajono uusinvalintatapajono = new VersioSet<Valintatapajono>(datastore.find(Valintatapajono.class,
                "valintatapajonooid", valintatapajonooid).asList()).haeUusinVersio();
        if (uusinvalintatapajono == null) {
            return Collections.<Jonosija> emptyList();
        }
        return uusinvalintatapajono.getJonosijat();
    }
      */
}
