package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

@Repository("hakijaryhmaDAO")
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

  @Qualifier("LaskentaAuditLog")
  @Autowired private LaskentaAuditLog auditLog;


  @Override
  public Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid) {
    return Optional.empty();
  }

  @Override
  public List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid) {
    return null;
  }

  @Override
  public void create(Hakijaryhma hakijaryhma, User auditUser) {
    saveJonosijat(hakijaryhma, auditUser);
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditUser,
        ValintaperusteetOperation.HAKIJARYHMA_PAIVITYS,
        ValintaResource.HAKIJARYHMA,
        hakijaryhma.getHakijaryhmatyyppikoodiUri(),
        Changes.addedDto(hakijaryhma));
    //datastore.save(hakijaryhma);
  }

  @Override
  public void createWithoutAuditLogging(Hakijaryhma hakijaryhma) {
    saveJonosijatWithoutAuditLogging(hakijaryhma);
    //datastore.save(hakijaryhma);
  }

  @Override
  public void poistaHakijaryhma(Hakijaryhma hakijaryhma) {
    List<String> jonosijaIdt = hakijaryhma.getJonosijaIdt();
    if (!jonosijaIdt.isEmpty()) {
      //datastore.delete(datastore.createQuery(Jonosija.class).field("_id").in(jonosijaIdt));
    }
    //datastore.delete(hakijaryhma);
  }

  private void saveJonosijat(Hakijaryhma ryhma, User auditUser) {
    ryhma.setJonosijat(
        ryhma.getJonosijat().stream()
            .map(jonosija -> saveJonosija(new Jonosija(), auditUser))
            .collect(Collectors.toList()));
  }

  private void saveJonosijatWithoutAuditLogging(Hakijaryhma ryhma) {
    /**ryhma.setJonosijaIdt(
        ryhma.getJonosijat().stream()
            .map(jonosija -> (ObjectId) saveJonosijaWithoutAuditLogging(jonosija).getId())
            .collect(Collectors.toList()));**/
  }

  private Jonosija saveJonosija(Jonosija jonosija, User auditUser) {
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditUser,
        ValintaperusteetOperation.JONOSIJA_PAIVITYS,
        ValintaResource.JONOSIJA,
        jonosija.getHakemusOid(),
        Changes.addedDto(jonosija));
    return jonosija; //datastore.save(jonosija);
  }

  private Jonosija saveJonosijaWithoutAuditLogging(Jonosija jonosija) {
    return jonosija; //datastore.save(jonosija);
  }

}
