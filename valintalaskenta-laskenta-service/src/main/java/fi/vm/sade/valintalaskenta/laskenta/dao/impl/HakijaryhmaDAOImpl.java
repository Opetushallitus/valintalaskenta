package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.HakijaryhmaRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.JonosijaRepository;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.hibernate.Session;
import org.jooq.DSLContext;
import org.jooq.impl.DSL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.persistence.EntityManager;
import javax.transaction.Transactional;

import static fi.vm.sade.valintalaskenta.laskenta.dao.QueryUtil.*;
import static org.jooq.impl.DSL.table;
import static org.jooq.impl.DSL.*;
import org.jooq.*;
import org.jooq.impl.*;

@Repository("hakijaryhmaDAO")
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

  @Autowired
  private LaskentaAuditLog auditLog;

  @Autowired
  private HakijaryhmaRepository repository;

  @Autowired
  private JonosijaRepository jonosijaRepository;

  @Autowired
  private EntityManager em;

  @Override
  public Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid) {
    /*List<Hakijaryhma> result = jooqQuery(em, ctx -> ctx.select()
            .from(table("Hakijaryhma"))
            //.join(table("Jonosija"))
            //.on(field("Jonosija.hakijaryhma").eq(field("Hakijaryhma.id")))
            .where(field("Hakijaryhma.hakijaryhma_oid").eq(hakijaryhmaOid)),
            Hakijaryhma.class);*/
    return repository.findByHakijaryhmaOid(hakijaryhmaOid);
  }

  @Override
  public List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid) {
    return null;
  }

  @Override
  @Transactional
  public void create(Hakijaryhma hakijaryhma, User auditUser) {
    saveJonosijat(hakijaryhma, auditUser);
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditUser,
        ValintaperusteetOperation.HAKIJARYHMA_PAIVITYS,
        ValintaResource.HAKIJARYHMA,
        hakijaryhma.getHakijaryhmatyyppiKoodiuri(),
        Changes.addedDto(hakijaryhma));
    repository.save(hakijaryhma);
  }

  @Override
  public void createWithoutAuditLogging(Hakijaryhma hakijaryhma) {
    saveJonosijatWithoutAuditLogging(hakijaryhma);
    repository.save(hakijaryhma);
  }

  @Override
  @Transactional
  public void poistaHakijaryhma(Hakijaryhma hakijaryhma) {
    List<UUID> jonosijaIdt = hakijaryhma.getJonosijaIdt();
    if (!jonosijaIdt.isEmpty()) {
      jonosijaRepository.deleteAllById(jonosijaIdt);
    }
    repository.delete(hakijaryhma);
  }

  private void saveJonosijat(Hakijaryhma ryhma, User auditUser) {
    ryhma.setJonosijat(
        ryhma.getJonosijat().stream()
            .map(jonosija -> saveJonosija(jonosija, auditUser))
            .collect(Collectors.toList()));
  }

  private void saveJonosijatWithoutAuditLogging(Hakijaryhma ryhma) {
    jonosijaRepository.saveAll(ryhma.getJonosijat());
    /**
     * ryhma.setJonosijaIdt( ryhma.getJonosijat().stream() .map(jonosija -> (ObjectId)
     * saveJonosijaWithoutAuditLogging(jonosija).getId()) .collect(Collectors.toList()));*
     */
  }

  private Jonosija saveJonosija(Jonosija jonosija, User auditUser) {
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditUser,
        ValintaperusteetOperation.JONOSIJA_PAIVITYS,
        ValintaResource.JONOSIJA,
        jonosija.getHakemusOid(),
        Changes.addedDto(jonosija));
    return jonosijaRepository.save(jonosija);
  }

  private Jonosija saveJonosijaWithoutAuditLogging(Jonosija jonosija) {
    return jonosijaRepository.save(jonosija);
  }
}
