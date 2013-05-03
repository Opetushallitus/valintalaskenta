package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.code.morphia.Datastore;

import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Repository
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

    @Autowired
    private Datastore datastore;

    public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
        List<VersiohallintaHakukohde> versiohallinnat = datastore.find(VersiohallintaHakukohde.class, "hakukohdeoid",
                hakukohdeoid).asList();
        if (versiohallinnat == null || versiohallinnat.isEmpty()) {
            LOGGER.info("versionhallinat tyhja");
            return Collections.emptyList();
        }
        LOGGER.info("versiohallinnat määrä on {}", versiohallinnat.size());
        List<Valinnanvaihe> valinnanvaiheet = new ArrayList<Valinnanvaihe>();
        for (VersiohallintaHakukohde versiohallinta : versiohallinnat) {
            valinnanvaiheet.add(versiohallinta.getHakukohteet().haeUusinVersio().getHakukohde().getValinnanvaihe());
        }
        return valinnanvaiheet;
    }
}
