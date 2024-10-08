package fi.vm.sade.valintalaskenta.ovara.ajastus;

import org.springframework.stereotype.Repository;

@Repository
public interface SiirtotiedostoProsessiRepository {

  SiirtotiedostoProsessi findLatestSuccessful();

  void persist(SiirtotiedostoProsessi sp);
}
