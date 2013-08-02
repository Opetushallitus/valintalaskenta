package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.code.morphia.Datastore;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintakoeOsallistuminenDAO;

@Repository
public class ValintakoeOsallistuminenDAOImpl implements ValintakoeOsallistuminenDAO {

    @Autowired
    private Datastore datastore;

    @Override
    public List<ValintakoeOsallistuminen> findAll() {
        return datastore.find(ValintakoeOsallistuminen.class).asList();
    }

    @Override
    public List<ValintakoeOsallistuminen> findByHakijaOid(String hakijaOid) {
        return datastore.find(ValintakoeOsallistuminen.class, "hakijaOid", hakijaOid).asList();
    }

    @Override
    public List<ValintakoeOsallistuminen> findByHakutoive(String hakukohdeOid) {
        return datastore.find(ValintakoeOsallistuminen.class).field("hakutoiveet.hakukohdeOid").equal(hakukohdeOid)
                .asList();
    }
}
