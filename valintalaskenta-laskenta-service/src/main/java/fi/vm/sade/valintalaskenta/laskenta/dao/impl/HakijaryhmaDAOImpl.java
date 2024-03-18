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
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import javax.transaction.Transactional;
import org.springframework.stereotype.Service;

@Service
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {

  private final LaskentaAuditLog auditLog;

  private final HakijaryhmaRepository repository;

  private final JonosijaRepository jonosijaRepository;

  public HakijaryhmaDAOImpl(
      LaskentaAuditLog auditLog,
      HakijaryhmaRepository repository,
      JonosijaRepository jonosijaRepository) {
    this.auditLog = auditLog;
    this.repository = repository;
    this.jonosijaRepository = jonosijaRepository;
  }

  @Override
  public Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid) {
    return repository.findByHakijaryhmaOid(hakijaryhmaOid);
  }

  @Override
  public List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid) {
    List<Hakijaryhma> ryhmat = repository.findAllByHakukohdeOid(hakukohdeOid);
    ryhmat.sort(Comparator.comparing(h -> h.prioriteetti));
    return ryhmat;
  }

  @Override
  @Transactional
  public void create(Hakijaryhma hakijaryhma, User auditUser) {
    hakijaryhma.id = null; // ensures insert and no update
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
  public void createWithoutAuditLogging(Hakijaryhma hakijaryhma) {
    hakijaryhma.id = null; // ensures insert and no update
    repository.save(hakijaryhma);
  }

  @Override
  @Transactional
  public void poistaHakijaryhma(Hakijaryhma hakijaryhma) {
    List<UUID> jonosijaIdt = hakijaryhma.jonosija.stream().map(Jonosija::getId).toList();
    if (!jonosijaIdt.isEmpty()) {
      jonosijaRepository.deleteAllById(jonosijaIdt);
    }
    repository.delete(hakijaryhma);
  }
}
