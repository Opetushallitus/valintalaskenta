package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintakoeOsallistuminenDAO;

import java.util.List;

import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosValintakoeOsallistuminenRepository;
import org.springframework.stereotype.Repository;

@Repository
public class TulosValintakoeOsallistuminenDAOImpl implements TulosValintakoeOsallistuminenDAO {

  private final TulosValintakoeOsallistuminenRepository repo;

  public TulosValintakoeOsallistuminenDAOImpl(TulosValintakoeOsallistuminenRepository repo) {
    this.repo = repo;
  }


  @Override
  public ValintakoeOsallistuminen findByHakemusOid(String hakemusOid) {
    return repo.findValintakoeOsallistuminenByHakemusOid(hakemusOid).orElse(null);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoive(String hakukohdeOid) {
    return repo.findDistinctValintakoeOsallistuminensByHakutoive(hakukohdeOid);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakutoiveet(List<String> hakukohdeOids) {
    return repo.findDistinctValintakoeOsallistuminensByHakutoiveet(hakukohdeOids);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakijaOids(List<String> hakijaOids) {
    return repo.findDistinctValintakoeOsallistuminensByHakijaOidIn(hakijaOids);
  }

  @Override
  public List<ValintakoeOsallistuminen> findByHakuAndOsallistuminen(
      String hakuOid, Osallistuminen osallistuminen) {
    return repo.findDistinctByHakuAndOsallistuminen(hakuOid, osallistuminen);
  }

}
