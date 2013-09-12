package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;
import fi.vm.sade.valintalaskenta.tulos.dao.HarkinnanvarainenHyvaksyminenDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 12.9.2013
 * Time: 14:27
 * To change this template use File | Settings | File Templates.
 */
@Repository("HarkinnanvarainenHyvaksyminenDAO")
public class HarkinnanvarainenHyvaksyminenDAOImpl implements HarkinnanvarainenHyvaksyminenDAO {

    @Autowired
    private Datastore datastore;

    @Override
    public HarkinnanvarainenHyvaksyminen haeHarkinnanvarainenHyvaksyminen( String hakukohdeOid, String hakemusOid) {
        return datastore.find(HarkinnanvarainenHyvaksyminen.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .field("hakemusOid").equal(hakemusOid)
                .get();
    }

    @Override
    public void tallennaHarkinnanvarainenHyvaksyminen(HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen) {
        datastore.save(harkinnanvarainenHyvaksyminen);
    }

    @Override
    public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvarainenHyvaksyminen(String hakukohdeOid) {
        return datastore.find(HarkinnanvarainenHyvaksyminen.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .asList();
    }
}
