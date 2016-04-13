package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import org.mongodb.morphia.Datastore;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintakoeOsallistuminenDAO;

@Repository
public class ValintakoeOsallistuminenDAOImpl implements ValintakoeOsallistuminenDAO {
    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @Override
    public ValintakoeOsallistuminen findByHakemusOid(String hakemusOid) {
        return datastore.find(ValintakoeOsallistuminen.class, "hakemusOid", hakemusOid).get();
    }

    @Override
    public List<ValintakoeOsallistuminen> findByHakutoive(String hakukohdeOid) {
        return datastore.find(ValintakoeOsallistuminen.class).field("hakutoiveet.hakukohdeOid").equal(hakukohdeOid).asList();
    }

    @Override
    public List<ValintakoeOsallistuminen> findByHakutoiveet(List<String> hakukohdeOids) {
        return datastore.find(ValintakoeOsallistuminen.class).field("hakutoiveet.hakukohdeOid").in(hakukohdeOids).asList();
    }

    @Override
    public List<ValintakoeOsallistuminen> findByHakuAndOsallistuminen(String hakuOid, Osallistuminen osallistuminen) {
        return datastore
                .find(ValintakoeOsallistuminen.class)
                .field("hakuOid")
                .equal(hakuOid)
                .field("hakutoiveet.valinnanVaiheet.valintakokeet.osallistuminenTulos.osallistuminen")
                .equal(osallistuminen).asList();
    }
}