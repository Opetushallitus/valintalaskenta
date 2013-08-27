package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.Nullable;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.code.morphia.Datastore;
import com.google.common.base.Function;
import com.google.common.collect.Collections2;

import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.tulos.dao.HakemusTulosDAO;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Hakemuslähtöistä tulosten hakua
 * 
 */
@Repository("hakemusTulosDAO")
public class HakemusTulosDAOImpl implements HakemusTulosDAO {

    @Autowired
    private Datastore datastore;

    @Override
    public List<Valintatapajono> findByHakemusOid(String hakemusOid) {
        return datastore.find(Valintatapajono.class).field("jonosijat.hakemusoid").equal(hakemusOid).order("-versio")
                .limit(1).asList();
    }

    @Override
    public Set<String> findValintatapajonoOidsByHakemusOid(String hakemusOid) {
        Collection<String> oidit = Collections2.transform(findByHakemusOid(hakemusOid),
                new Function<Valintatapajono, String>() {
                    public String apply(@Nullable Valintatapajono jono) {
                        return jono.getOid();
                    }
                });
        return new HashSet<String>(oidit);
    }

    @Override
    public List<VersiohallintaHakukohde> findByValinnanvaiheOid(String hakuOid) {
        return datastore.find(VersiohallintaHakukohde.class).field("hakuoid").equal(hakuOid).asList();
    }
}
