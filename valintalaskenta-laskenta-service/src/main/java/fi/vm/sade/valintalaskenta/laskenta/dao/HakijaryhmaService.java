package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaEntity;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;

import java.util.List;
import java.util.Optional;

public interface HakijaryhmaService {

  Optional<HakijaryhmaEntity> haeHakijaryhma(String hakijaryhmaOid);

  Optional<Hakijaryhma> haeHakijaryhmaLite(String hakijaryhmaOid);

  List<HakijaryhmaEntity> haeHakijaryhmat(String hakukohdeOid);

  void create(Hakijaryhma hakijaryhma, User auditUser);

  void create(HakijaryhmaEntity hakijaryhma, User auditUser);

  void createWithoutAuditLogging(HakijaryhmaEntity hakijaryhma);

  void poistaHakijaryhma(HakijaryhmaEntity hakijaryhma);
}
