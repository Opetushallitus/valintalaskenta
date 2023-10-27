package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import java.util.List;
import java.util.Optional;

public interface HakijaryhmaDAO {

  Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid);

  List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid);

  void create(Hakijaryhma hakijaryhma, User auditUser);

  void createWithoutAuditLogging(Hakijaryhma hakijaryhma);

  void poistaHakijaryhma(Hakijaryhma hakijaryhma);
}
