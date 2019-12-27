package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.List;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import org.mongodb.morphia.Key;
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

    @Autowired
    private LaskentaAuditLog auditLog;

    @Override
    public HarkinnanvarainenHyvaksyminen haeHarkinnanvarainenHyvaksyminen(String hakukohdeOid, String hakemusOid) {
        return datastore.find(HarkinnanvarainenHyvaksyminen.class)
                .field("hakukohdeOid").equal(hakukohdeOid).field("hakemusOid")
                .equal(hakemusOid).get();
    }

    @Override
    public void tallennaHarkinnanvarainenHyvaksyminen(HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen, User auditUser) {
        saveHarkinnanvarainenHyvaksyminen(harkinnanvarainenHyvaksyminen, auditUser);
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

    private Key<HarkinnanvarainenHyvaksyminen> saveHarkinnanvarainenHyvaksyminen(HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen, User auditUser) {
        auditLog.log(LaskentaAudit.AUDIT,
                auditUser,
                ValintaperusteetOperation.HARKINNANVARAINEN_HYVAKSYMINEN_PAIVITYS,
                ValintaResource.HARKINNANVARAINEN_HYVAKSYMINEN,
                harkinnanvarainenHyvaksyminen.getHakemusOid(),
                Changes.addedDto(harkinnanvarainenHyvaksyminen));
        return datastore.save(harkinnanvarainenHyvaksyminen);
    }

}
