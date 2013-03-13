package fi.vm.sade.valintalaskenta.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.dao.HakukohdeDAO;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Repository
public class HakukohdeDAOImpl implements HakukohdeDAO {

    @Autowired
    private Datastore datastore;

    public List<Versioituhakukohde> readAll() {
        return filterUusinVersioOnly(datastore.find(VersiohallintaHakukohde.class).asList());
    }

    public List<Versioituhakukohde> readByHakuOid(String hakuoid) {
        return filterUusinVersioOnly(datastore.find(VersiohallintaHakukohde.class, "hakuoid", hakuoid).asList());
    }

    private List<Versioituhakukohde> filterUusinVersioOnly(List<VersiohallintaHakukohde> versiohallinnat) {
        List<Versioituhakukohde> hakukohteet = new ArrayList<Versioituhakukohde>();
        for (VersiohallintaHakukohde versiohallinta : versiohallinnat) {
            hakukohteet.add(versiohallinta.getHakukohteet().haeUusinVersio());
        }
        return hakukohteet;
    }

}
