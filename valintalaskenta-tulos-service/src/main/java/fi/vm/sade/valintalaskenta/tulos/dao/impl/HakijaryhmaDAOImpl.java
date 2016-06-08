package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.tulos.dao.HakijaryhmaDAO;
import org.mongodb.morphia.Datastore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @Override
    public List<Hakijaryhma> readByHakukohdeOid(String hakukohdeoid) {
        List<Hakijaryhma> ryhmat = datastore.createQuery(Hakijaryhma.class)
                .field("hakukohdeOid").equal(hakukohdeoid)
                .asList();
        ryhmat.forEach(ryhma -> {
            ryhma.setJonosijat(datastore.createQuery(Jonosija.class)
                    .field("_id").in(ryhma.getJonosijaIdt())
                    .asList());
        });
        return ryhmat;
    }
}
