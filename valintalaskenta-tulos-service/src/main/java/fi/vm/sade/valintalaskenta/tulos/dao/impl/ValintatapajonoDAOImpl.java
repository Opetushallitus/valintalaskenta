package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintatapajonoDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

/**
 * @author Jussi Jartamo
 */
@Repository
public class ValintatapajonoDAOImpl implements ValintatapajonoDAO {

    @Autowired
    private Datastore datastore;

    /*
    public List<Valintatapajono> readByValinnanvaiheOid(String valinnanvaiheOid) {
        VersiohallintaHakukohde versiohallinta = datastore.find(VersiohallintaHakukohde.class, "valinnanvaiheoid",
                valinnanvaiheOid).get();
        if (versiohallinta == null) {
            return Collections.emptyList();
        }
        return versiohallinta.getHakukohteet().haeUusinVersio().getHakukohde().getValinnanvaihe().getValintatapajono();
    }
      */
    @Override
    public Valintatapajono findByValintatapajonoOidHakemusOidAndJarjestyskriteeriPrioriteetti(String valintatapajonoOid, String hakemusOid, Integer jarjestyskriteeriPrioriteetti) {
        return datastore.find(Valintatapajono.class)
                .filter("valintatapajonooid", valintatapajonoOid)
                .filter("jonosijat.hakemusoid", hakemusOid)
                .filter("jonosijat.jarjestyskriteerit." + jarjestyskriteeriPrioriteetti + " exists", true)
                .order("-versio").limit(1)
                .get();
    }

    @Override
    public Valintatapajono findByOid(String valintatapajonoOid) {
        return datastore.find(Valintatapajono.class)
                .filter("valintatapajonooid", valintatapajonoOid)
                .order("-versio").limit(1)
                .get();
    }

}
