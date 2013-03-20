package fi.vm.sade.valintalaskenta.dao.impl;

import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.code.morphia.Datastore;

import fi.vm.sade.valintalaskenta.dao.ValintatapajonoDAO;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Repository
public class ValintatapajonoDAOImpl implements ValintatapajonoDAO {

    @Autowired
    private Datastore datastore;

    public List<Valintatapajono> readByValinnanvaiheOid(String valinnanvaiheOid) {
        VersiohallintaHakukohde versiohallinta = datastore.find(VersiohallintaHakukohde.class, "valinnanvaiheoid",
                valinnanvaiheOid).get();
        if (versiohallinta == null) {
            return Collections.emptyList();
        }
        return versiohallinta.getHakukohteet().haeUusinVersio().getHakukohde().getValinnanvaihe().getValintatapajono();
    }

}
