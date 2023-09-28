package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaEntity;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaService;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.HakijaryhmaRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.JonosijaRepository;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;

@Service
public class HakijaryhmaServiceImpl implements HakijaryhmaService {
  private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaServiceImpl.class);

  private final LaskentaAuditLog auditLog;

  private final HakijaryhmaRepository repository;

  private final JonosijaRepository jonosijaRepository;

  public HakijaryhmaServiceImpl(LaskentaAuditLog auditLog, HakijaryhmaRepository repository, JonosijaRepository jonosijaRepository) {
    this.auditLog = auditLog;
    this.repository = repository;
    this.jonosijaRepository = jonosijaRepository;
  }


  @Override
  public Optional<HakijaryhmaEntity> haeHakijaryhma(String hakijaryhmaOid) {
    /*List<Hakijaryhma> result = jooqQuery(em, ctx -> ctx.select()
            .from(table("Hakijaryhma"))
            //.join(table("Jonosija"))
            //.on(field("Jonosija.hakijaryhma").eq(field("Hakijaryhma.id")))
            .where(field("Hakijaryhma.hakijaryhma_oid").eq(hakijaryhmaOid)),
            Hakijaryhma.class);*/
    return Optional.empty();
    //return repository.findByHakijaryhmaOid(hakijaryhmaOid);
  }

  @Override
  public Optional<Hakijaryhma> haeHakijaryhmaLite(String hakijaryhmaOid) {
    return repository.findByHakijaryhmaOid(hakijaryhmaOid);
  }

  @Override
  public List<HakijaryhmaEntity> haeHakijaryhmat(String hakukohdeOid) {
    return null;
  }

  @Transactional
  @Override
  public void create(Hakijaryhma hakijaryhma, User auditUser) {
    auditLog.log(
            LaskentaAudit.AUDIT,
            auditUser,
            ValintaperusteetOperation.HAKIJARYHMA_PAIVITYS,
            ValintaResource.HAKIJARYHMA,
            hakijaryhma.hakijaryhmatyyppiKoodiuri,
            Changes.addedDto(hakijaryhma));
    repository.save(hakijaryhma);
  }

  @Override
  @Transactional
  public void create(HakijaryhmaEntity hakijaryhma, User auditUser) {
    saveJonosijat(hakijaryhma, auditUser);
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditUser,
        ValintaperusteetOperation.HAKIJARYHMA_PAIVITYS,
        ValintaResource.HAKIJARYHMA,
        hakijaryhma.getHakijaryhmatyyppiKoodiuri(),
        Changes.addedDto(hakijaryhma));
    //repository.save(hakijaryhma);
  }

  @Override
  public void createWithoutAuditLogging(HakijaryhmaEntity hakijaryhma) {
    saveJonosijatWithoutAuditLogging(hakijaryhma);
    //repository.save(hakijaryhma);
  }

  @Override
  @Transactional
  public void poistaHakijaryhma(HakijaryhmaEntity hakijaryhma) {
    List<UUID> jonosijaIdt = hakijaryhma.getJonosijaIdt();
    if (!jonosijaIdt.isEmpty()) {
      jonosijaRepository.deleteAllById(jonosijaIdt);
    }
   //repository.delete(hakijaryhma);
  }

  private void saveJonosijat(HakijaryhmaEntity ryhma, User auditUser) {
    ryhma.setJonosijat(
        ryhma.getJonosijat().stream()
            .map(jonosija -> saveJonosija(jonosija, auditUser))
            .collect(Collectors.toList()));
  }

  private void saveJonosijatWithoutAuditLogging(HakijaryhmaEntity ryhma) {
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
