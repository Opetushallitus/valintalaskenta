package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import org.mongodb.morphia.Datastore;

import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;
import fi.vm.sade.valintalaskenta.tulos.dao.HarkinnanvarainenHyvaksyminenDAO;

@Repository("HarkinnanvarainenHyvaksyminenDAO")
public class HarkinnanvarainenHyvaksyminenDAOImpl implements HarkinnanvarainenHyvaksyminenDAO {
    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @Override
    public HarkinnanvarainenHyvaksyminen haeHarkinnanvarainenHyvaksyminen(String hakukohdeOid, String hakemusOid) {
        return datastore.find(HarkinnanvarainenHyvaksyminen.class)
                .field("hakukohdeOid").equal(hakukohdeOid).field("hakemusOid")
                .equal(hakemusOid).get();
    }

    @Override
    public void tallennaHarkinnanvarainenHyvaksyminen(HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen) {
        datastore.save(harkinnanvarainenHyvaksyminen);
    }

    @Override
    public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvarainenHyvaksyminen(String hakukohdeOid) {
        return datastore.find(HarkinnanvarainenHyvaksyminen.class)
                .field("hakukohdeOid").equal(hakukohdeOid).asList();
    }

    @Override
    public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisetHyvaksymisetHaulle(String hakuOid) {
        return datastore.find(HarkinnanvarainenHyvaksyminen.class)
                .field("hakuOid").equal(hakuOid).asList();
    }

    @Override
    public List<HarkinnanvarainenHyvaksyminen> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
        return datastore.find(HarkinnanvarainenHyvaksyminen.class)
                .field("hakuOid").equal(hakuOid).field("hakemusOid")
                .equal(hakemusOid).asList();
    }
}
