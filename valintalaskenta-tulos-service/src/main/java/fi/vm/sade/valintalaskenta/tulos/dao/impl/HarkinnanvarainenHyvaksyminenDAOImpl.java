package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;
import fi.vm.sade.valintalaskenta.tulos.dao.HarkinnanvarainenHyvaksyminenDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosHarkinnanvarainenHyvaksyminenRepository;
import java.util.List;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class HarkinnanvarainenHyvaksyminenDAOImpl implements HarkinnanvarainenHyvaksyminenDAO {

  private final TulosHarkinnanvarainenHyvaksyminenRepository repo;

  public HarkinnanvarainenHyvaksyminenDAOImpl(TulosHarkinnanvarainenHyvaksyminenRepository repo) {
    this.repo = repo;
  }

  @Override
  public HarkinnanvarainenHyvaksyminen haeHarkinnanvarainenHyvaksyminen(
      String hakukohdeOid, String hakemusOid) {
    return repo.findHarkinnanvarainenHyvaksyminenByHakukohdeOidAndHakemusOid(
            hakukohdeOid, hakemusOid)
        .orElse(null);
  }

  @Override
  @Transactional
  public void tallennaHarkinnanvarainenHyvaksyminen(
      HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen) {
    repo.save(harkinnanvarainenHyvaksyminen);
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvarainenHyvaksyminen(String hakukohdeOid) {
    return repo.findHarkinnanvarainenHyvaksyminensByHakukohdeOid(hakukohdeOid);
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisetHyvaksymisetHaulle(
      String hakuOid) {
    return repo.findHarkinnanvarainenHyvaksyminensByHakuOid(hakuOid);
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> readByHakuOidAndHakemusOid(
      String hakuOid, String hakemusOid) {
    return repo.findHarkinnanvarainenHyvaksyminensByHakuOidAndHakemusOid(hakuOid, hakemusOid);
  }
}
