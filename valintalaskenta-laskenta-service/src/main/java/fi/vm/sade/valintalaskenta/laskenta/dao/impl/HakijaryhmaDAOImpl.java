package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Key;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Repository("hakijaryhmaDAO")
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {

    @Autowired
    private Datastore datastore;

    @PostConstruct
    public void ensureIndexes() {
        datastore.ensureIndexes(Hakijaryhma.class);
    }

    @Override
    public Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid) {
        Optional<Hakijaryhma> ryhmaOpt = Optional.ofNullable(datastore.find(Hakijaryhma.class)
                .field("hakijaryhmaOid").equal(hakijaryhmaOid)
                .get());
        ryhmaOpt.ifPresent(ryhma -> {
            ryhma.setJonosijat(datastore.createQuery(Jonosija.class).field("_id").in(ryhma.getJonosijaIdt()).asList());
        });
        return ryhmaOpt;
    }

    @Override
    public List<Hakijaryhma> haeHakijaryhmatPrioriteetilla(String hakukohdeOid, int prioriteetti) {
        List<Hakijaryhma> ryhmat = datastore.find(Hakijaryhma.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .field("prioriteetti").equal(prioriteetti)
                .asList();
        ryhmat.forEach(ryhma -> {
            ryhma.setJonosijat(datastore.createQuery(Jonosija.class).field("_id").in(ryhma.getJonosijaIdt()).asList());
        });
        return ryhmat;
    }

    @Override
    public List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid) {
        List<Hakijaryhma> ryhmat = datastore.find(Hakijaryhma.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .asList();
        ryhmat.forEach(ryhma -> {
            ryhma.setJonosijat(datastore.createQuery(Jonosija.class).field("_id").in(ryhma.getJonosijaIdt()).asList());
        });
        return ryhmat;
    }

    @Override
    public void create(Hakijaryhma hakijaryhma) {
        hakijaryhma.setJonosijaIdt(hakijaryhma.getJonosijat().stream()
                .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
                .collect(Collectors.toList()));
        datastore.save(hakijaryhma);
    }

    @Override
    public void poistaHakijaryhma(Hakijaryhma hakijaryhma) {
        hakijaryhma.getJonosijat().forEach(datastore::delete);
        datastore.delete(hakijaryhma);
    }
}
