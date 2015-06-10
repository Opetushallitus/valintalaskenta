package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import org.mongodb.morphia.Datastore;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.List;
import java.util.Optional;

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
        return Optional.ofNullable(datastore.find(Hakijaryhma.class)
                .field("hakijaryhmaOid").equal(hakijaryhmaOid)
                .get());
    }

    @Override
    public List<Hakijaryhma> haeHakijaryhmatPrioriteetilla(String hakukohdeOid, int prioriteetti) {
        return datastore.find(Hakijaryhma.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .field("prioriteetti").equal(prioriteetti)
                .asList();
    }

    @Override
    public List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid) {
        return datastore.find(Hakijaryhma.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .asList();
    }

    @Override
    public void create(Hakijaryhma hakijaryhma) {
        datastore.save(hakijaryhma);
    }

    @Override
    public void poistaHakijaryhma(Hakijaryhma hakijaryhma) {
        datastore.delete(hakijaryhma);
    }
}
