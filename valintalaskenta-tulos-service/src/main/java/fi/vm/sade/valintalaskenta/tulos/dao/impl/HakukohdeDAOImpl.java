package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.tulos.dao.HakukohdeDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

/**
 *
 * @author Jussi Jartamo
 *
 */
@Repository
public class HakukohdeDAOImpl implements HakukohdeDAO {

    @Autowired
    private Datastore datastore;

    /*
    public List<Versioituhakukohde> readAll() {
        return filterUusinVersioOnly(datastore.find(VersiohallintaHakukohde.class).asList());
    } */

    /**
     * This is plain and simple shit, refactor whole application domain
     * @param valintatapajono
     * @return
     */
    @Override
    public VersiohallintaHakukohde findByValintatapajono(Valintatapajono valintatapajono) {




        return  datastore.find(VersiohallintaHakukohde.class)
                .filter("hakukohteet.hakukohde.valinnanvaihe.valintatapajono", valintatapajono)
                .get();

    }



   /* public List<Versioituhakukohde> readByHakuOid(String hakuoid) {
        return filterUusinVersioOnly(datastore.find(VersiohallintaHakukohde.class, "hakuoid", hakuoid).asList());
    }*/

    /*private List<Versioituhakukohde> filterUusinVersioOnly(List<VersiohallintaHakukohde> versiohallinnat) {
        List<Versioituhakukohde> hakukohteet = new ArrayList<Versioituhakukohde>();
        for (VersiohallintaHakukohde versiohallinta : versiohallinnat) {
            hakukohteet.add(versiohallinta.getHakukohteet().haeUusinVersio());
        }
        return hakukohteet;
    } */

}
