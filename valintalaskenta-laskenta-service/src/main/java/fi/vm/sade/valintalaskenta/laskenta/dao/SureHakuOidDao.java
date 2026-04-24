package fi.vm.sade.valintalaskenta.laskenta.dao;

import java.util.Set;

/** Haut joille käytetään Suoritusrekisterin tietoja Suorituspalvelun sijaan. */
public interface SureHakuOidDao {

  /** Palauttaa haku-OIDit joille käytetään Suoritusrekisterin tietoja. */
  Set<String> haeSureHakuOidit();
}
