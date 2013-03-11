package fi.vm.sade.valintalaskenta.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.code.morphia.Datastore;
import com.google.common.collect.Sets;

import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.service.ValintalaskentaTulosService;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Service
public class ValintalaskentaTulosServiceImpl implements ValintalaskentaTulosService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValintalaskentaTulosServiceImpl.class);

    @Autowired
    private Datastore datastore;

    public List<Hakukohde> haeHakukohteet() {
        List<VersiohallintaHakukohde> versiohallinnat = datastore.find(VersiohallintaHakukohde.class).asList();
        List<Hakukohde> hakukohteet = new ArrayList<Hakukohde>();
        for (VersiohallintaHakukohde versiohallinta : versiohallinnat) {
            hakukohteet.add(versiohallinta.getHakukohteet().last().getHakukohde());
        }
        return hakukohteet;
    }

    public List<Hakukohde> haeHakukohteetHaulle(String hakuoid) {
        List<VersiohallintaHakukohde> versiohallinnat = datastore.find(VersiohallintaHakukohde.class, "hakuoid",
                hakuoid).asList();
        List<Hakukohde> hakukohteet = new ArrayList<Hakukohde>();
        for (VersiohallintaHakukohde versiohallinta : versiohallinnat) {
            hakukohteet.add(versiohallinta.getHakukohteet().last().getHakukohde());
        }
        return hakukohteet;
    }

    public List<Jarjestyskriteeritulos> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid) {
        Valintatapajono uusinvalintatapajono = Sets.newTreeSet(
                datastore.find(Valintatapajono.class, "valintatapajonooid", valintatapajonooid).asList()).last();
        if (uusinvalintatapajono == null) {
            return Collections.<Jarjestyskriteeritulos> emptyList();
        }
        return uusinvalintatapajono.getJarjestyskriteeritulokset();
    }

    public List<Valinnanvaihe> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {
        List<VersiohallintaHakukohde> versiohallinnat = datastore.find(VersiohallintaHakukohde.class, "hakukohdeoid",
                hakukohdeoid).asList();
        if (versiohallinnat == null || versiohallinnat.isEmpty()) {
            LOGGER.debug("Hakukohteita oid:llä '{}' ei löytynyt! Annetaan palautteena tyhjä lista!", hakukohdeoid);
            return Collections.emptyList();
        }
        List<Valinnanvaihe> valinnanvaiheet = new ArrayList<Valinnanvaihe>();
        for (VersiohallintaHakukohde versiohallinta : versiohallinnat) {
            valinnanvaiheet.add(versiohallinta.getHakukohteet().last().getHakukohde().getValinnanvaihe());
        }
        return valinnanvaiheet;
    }

    public List<Valintatapajono> haeValintatapajonoValinnanvaiheelle(String valinnanvaiheoid) {
        VersiohallintaHakukohde versiohallinta = datastore.find(VersiohallintaHakukohde.class, "valinnanvaiheoid",
                valinnanvaiheoid).get();
        if (versiohallinta == null) {
            return Collections.emptyList();
        }
        return versiohallinta.getHakukohteet().last().getHakukohde().getValinnanvaihe().getValintatapajono();
    }

}
