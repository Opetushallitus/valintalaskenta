package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.mongodb.*;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.dao.HakijaryhmaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.MongoMapReduceUtil;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.mapping.Mapper;
import org.mongodb.morphia.mapping.cache.DefaultEntityCache;
import org.mongodb.morphia.query.UpdateOperations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

@Repository
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @Override
    public List<Hakijaryhma> readByHakukohdeOid(String hakukohdeoid) {
        return datastore.createQuery(Hakijaryhma.class).field("hakukohdeOid").equal(hakukohdeoid).asList();
    }
}
