package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;

// TODO: poista tai toteuta OK-384 yhteydessä

public interface JarjestyskriteerihistoriaDAO {

  void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria);

  void delete(String id);

  Jarjestyskriteerihistoria hae(String id);
}
