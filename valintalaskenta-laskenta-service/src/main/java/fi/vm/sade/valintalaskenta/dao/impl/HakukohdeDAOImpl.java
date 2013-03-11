package fi.vm.sade.valintalaskenta.dao.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.code.morphia.Datastore;

import fi.vm.sade.valintalaskenta.dao.HakukohdeDAO;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Repository
public class HakukohdeDAOImpl implements HakukohdeDAO {

    @Autowired
    private Datastore datastore;

    public List<Hakukohde> readAll() {
        return filterUusinVersioOnly(datastore.find(VersiohallintaHakukohde.class).asList());
    }

    public List<Hakukohde> readByHakuOid(String hakuoid) {
        return filterUusinVersioOnly(datastore.find(VersiohallintaHakukohde.class, "hakuoid", hakuoid).asList());
    }

    private List<Hakukohde> filterUusinVersioOnly(List<VersiohallintaHakukohde> versiohallinnat) {
        List<Hakukohde> hakukohteet = new ArrayList<Hakukohde>();
        for (VersiohallintaHakukohde versiohallinta : versiohallinnat) {
            hakukohteet.add(versiohallinta.getHakukohteet().haeUusinVersio().getHakukohde());
        }
        return hakukohteet;
    }

}
