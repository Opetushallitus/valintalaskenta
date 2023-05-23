package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import static dev.morphia.query.filters.Filters.and;
import static dev.morphia.query.filters.Filters.eq;

import dev.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;
import fi.vm.sade.valintalaskenta.tulos.dao.HarkinnanvarainenHyvaksyminenDAO;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

@Repository("HarkinnanvarainenHyvaksyminenDAO")
public class HarkinnanvarainenHyvaksyminenDAOImpl implements HarkinnanvarainenHyvaksyminenDAO {
  @Qualifier("datastore2")
  @Autowired
  private Datastore datastore;

  @Override
  public HarkinnanvarainenHyvaksyminen haeHarkinnanvarainenHyvaksyminen(
      String hakukohdeOid, String hakemusOid) {
    return datastore
        .find(HarkinnanvarainenHyvaksyminen.class)
        .filter(and(eq("hakukohdeOid", hakukohdeOid), eq("hakemusOid", hakemusOid)))
        .first();
  }

  @Override
  public void tallennaHarkinnanvarainenHyvaksyminen(
      HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen) {
    datastore.save(harkinnanvarainenHyvaksyminen);
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvarainenHyvaksyminen(String hakukohdeOid) {
    return datastore
        .find(HarkinnanvarainenHyvaksyminen.class)
        .filter(eq("hakukohdeOid", hakukohdeOid))
        .stream()
        .collect(Collectors.toList());
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisetHyvaksymisetHaulle(
      String hakuOid) {
    return datastore
        .find(HarkinnanvarainenHyvaksyminen.class)
        .filter(eq("hakuOid", hakuOid))
        .stream()
        .collect(Collectors.toList());
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> readByHakuOidAndHakemusOid(
      String hakuOid, String hakemusOid) {
    return datastore
        .find(HarkinnanvarainenHyvaksyminen.class)
        .filter(and(eq("hakuOid", hakuOid), eq("hakemusOid", hakemusOid)))
        .stream()
        .collect(Collectors.toList());
  }
}
