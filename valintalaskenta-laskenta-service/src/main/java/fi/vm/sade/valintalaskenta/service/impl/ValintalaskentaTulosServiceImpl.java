package fi.vm.sade.valintalaskenta.service.impl;

import java.util.Collections;
import java.util.List;

import javax.annotation.Nonnull;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.code.morphia.Datastore;
import com.google.common.base.Function;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
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
        List<VersiohallintaHakukohde> versiohallinta = datastore.find(VersiohallintaHakukohde.class).asList();
        return Lists.newArrayList(Iterables.transform(versiohallinta,
                new Function<VersiohallintaHakukohde, Hakukohde>() {
                    public Hakukohde apply(@Nonnull VersiohallintaHakukohde input) {
                        assert (!input.getHakukohteet().isEmpty());
                        return input.getHakukohteet().last().getHakukohde();
                    }
                }));
    }

    public List<Hakukohde> haeHakukohteetHaulle(String hakuoid) {
        List<VersiohallintaHakukohde> versiohallinta = datastore
                .find(VersiohallintaHakukohde.class, "hakuoid", hakuoid).asList();
        return Lists.newArrayList(Iterables.transform(versiohallinta,
                new Function<VersiohallintaHakukohde, Hakukohde>() {
                    public Hakukohde apply(@Nonnull VersiohallintaHakukohde input) {
                        assert (!input.getHakukohteet().isEmpty());
                        return input.getHakukohteet().last().getHakukohde();
                    }
                }));
    }

    public List<Jarjestyskriteeritulos> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid) {
        Valintatapajono uusinvalintatapajono = Sets.newTreeSet(
                datastore.find(Valintatapajono.class, "valintatapajonooid", valintatapajonooid).asList()).last();

        return uusinvalintatapajono == null ? Collections.<Jarjestyskriteeritulos> emptyList() : uusinvalintatapajono
                .getJarjestyskriteeritulokset();
    }

    public List<Valinnanvaihe> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {
        List<VersiohallintaHakukohde> versiohallinta = datastore.find(VersiohallintaHakukohde.class, "hakukohdeoid",
                hakukohdeoid).asList();
        if (versiohallinta == null || versiohallinta.isEmpty()) {
            LOGGER.debug("Hakukohteita oid:llä '{}' ei löytynyt! Annetaan palautteena tyhjä lista!", hakukohdeoid);
            return Collections.emptyList();
        }
        return Lists.newArrayList(Iterables.transform(versiohallinta,
                new Function<VersiohallintaHakukohde, Valinnanvaihe>() {
                    public Valinnanvaihe apply(@Nonnull VersiohallintaHakukohde input) {
                        assert (!input.getHakukohteet().isEmpty());
                        return input.getHakukohteet().last().getHakukohde().getValinnanvaihe();
                    }
                }));
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
