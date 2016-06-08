package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import org.mongodb.morphia.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Repository("valinnanvaiheDAO")
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
    @Autowired
    private Datastore datastore;

    @PostConstruct
    public void ensureIndexes() {
        datastore.ensureIndexes(Valinnanvaihe.class);
        datastore.ensureIndexes(Valintatapajono.class);
    }

    @Override
    public Valinnanvaihe haeEdeltavaValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        Valinnanvaihe edellinen = null;

        if (jarjestysnumero > 0) {
            edellinen = datastore.find(Valinnanvaihe.class)
                    .field("hakuOid").equal(hakuOid)
                    .field("hakukohdeOid").equal(hakukohdeOid)
                    .field("jarjestysnumero").equal(jarjestysnumero - 1)
                    .limit(1)
                    .get();
            populateJonosijat(edellinen);
        }

        return edellinen;
    }

    @Override
    public Valinnanvaihe haeViimeisinValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        Valinnanvaihe edellinen = null;
        if (jarjestysnumero > 0) {
            edellinen = datastore.find(Valinnanvaihe.class)
                    .field("hakuOid").equal(hakuOid)
                    .field("hakukohdeOid").equal(hakukohdeOid)
                    .field("jarjestysnumero").lessThan(jarjestysnumero)
                    .order("-jarjestysnumero")
                    .limit(1)
                    .get();
            populateJonosijat(edellinen);
        }
        return edellinen;
    }

    @Override
    public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
        Valinnanvaihe valinnanvaihe = datastore.find(Valinnanvaihe.class)
                .field("valinnanvaiheOid").equal(valinnanvaiheOid)
                .get();
        populateJonosijat(valinnanvaihe);
        return valinnanvaihe;
    }

    @Override
    public List<Valinnanvaihe> haeValinnanvaiheetJarjestysnumerolla(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        List<Valinnanvaihe> valinnanvaiheet = datastore.find(Valinnanvaihe.class)
                .field("hakuOid").equal(hakuOid)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .field("jarjestysnumero").equal(jarjestysnumero)
                .asList();
        valinnanvaiheet.forEach(this::populateJonosijat);
        return valinnanvaiheet;

    }

    @Override
    public void saveOrUpdate(Valinnanvaihe valinnanvaihe) {
        valinnanvaihe.getValintatapajonot().forEach(valintatapajono -> {
            saveJonosijat(valintatapajono);
            datastore.save(valintatapajono);
        });
        datastore.save(valinnanvaihe);
    }

    @Override
    public void poistaValinnanvaihe(Valinnanvaihe valinnanvaihe) {
        valinnanvaihe.getValintatapajonot().forEach(this::poistaJono);
        datastore.delete(valinnanvaihe);
    }

    @Override
    public void poistaJono(Valintatapajono jono) {
        datastore.delete(datastore.createQuery(Jonosija.class).field("_id").in(jono.getJonosijaIdt()));
        datastore.delete(jono);
    }

    private void saveJonosijat(Valintatapajono valintatapajono) {
        valintatapajono.setJonosijaIdt(valintatapajono.getJonosijat().stream()
                .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
                .collect(Collectors.toList()));
    }

    private void populateJonosijat(Valinnanvaihe valinnanvaihe) {
        if (null != valinnanvaihe) {
            valinnanvaihe.getValintatapajonot().forEach(valintatapajono -> {
                valintatapajono.setJonosijat(datastore.createQuery(Jonosija.class)
                        .field("_id").in(valintatapajono.getJonosijaIdt())
                        .asList());
            });
        }
    }
}
