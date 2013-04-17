package fi.vm.sade.valintalaskenta.dao.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.code.morphia.Datastore;

import fi.vm.sade.valintalaskenta.dao.VersiohallintaHakukohdeDAO;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Repository("VersiohallintaHakukohdeDAO")
public class VersiohallintaHakukohdeDAOImpl implements VersiohallintaHakukohdeDAO {

    @Autowired
    private Datastore datastore;

    public VersiohallintaHakukohde readByHakukohdeOidAndJarjestysnumero(String hakukohdeoid, Integer jarjestysnumero) {
        return datastore.find(VersiohallintaHakukohde.class, "hakukohdeoid", hakukohdeoid)
                .filter("jarjestysnumero", jarjestysnumero).get();
    }

    public List<VersiohallintaHakukohde> findTwoLatestByHakukohdeOid(String hakukohdeoid) {
        return datastore.find(VersiohallintaHakukohde.class, "hakukohdeoid", hakukohdeoid).order("jarjestysnumero")
                .limit(2).asList();
    }

    public void createOrUpdate(VersiohallintaHakukohde v) {
        datastore.save(v);
    }
}
