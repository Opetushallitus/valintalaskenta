package fi.vm.sade.valintalaskenta.dao.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.code.morphia.Datastore;

import fi.vm.sade.valintalaskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Repository
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {

    @Autowired
    private Datastore datastore;

    public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
        List<VersiohallintaHakukohde> versiohallinnat = datastore.find(VersiohallintaHakukohde.class, "hakukohdeoid",
                hakukohdeoid).asList();
        if (versiohallinnat == null || versiohallinnat.isEmpty()) {
            return Collections.emptyList();
        }
        List<Valinnanvaihe> valinnanvaiheet = new ArrayList<Valinnanvaihe>();
        for (VersiohallintaHakukohde versiohallinta : versiohallinnat) {
            valinnanvaiheet.add(versiohallinta.getHakukohteet().haeUusinVersio().getHakukohde().getValinnanvaihe());
        }
        return valinnanvaiheet;
    }
}
