package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;
import fi.vm.sade.valintalaskenta.tulos.dao.HarkinnanvarainenHyvaksyminenDAO;
import java.util.List;
import org.springframework.stereotype.Repository;

@Repository("HarkinnanvarainenHyvaksyminenDAO")
public class HarkinnanvarainenHyvaksyminenDAOImpl implements HarkinnanvarainenHyvaksyminenDAO {

  @Override
  public HarkinnanvarainenHyvaksyminen haeHarkinnanvarainenHyvaksyminen(
      String hakukohdeOid, String hakemusOid) {
    return null;
 /*   return datastore
        .find(HarkinnanvarainenHyvaksyminen.class)
        .field("hakukohdeOid")
        .equal(hakukohdeOid)
        .field("hakemusOid")
        .equal(hakemusOid)
        .get();*/
  }

  @Override
  public void tallennaHarkinnanvarainenHyvaksyminen(
      HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen) {
    //datastore.save(harkinnanvarainenHyvaksyminen);
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvarainenHyvaksyminen(String hakukohdeOid) {
    return null;
/*    return datastore
        .find(HarkinnanvarainenHyvaksyminen.class)
        .field("hakukohdeOid")
        .equal(hakukohdeOid)
        .asList();*/
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisetHyvaksymisetHaulle(
      String hakuOid) {
    return null;
/*    return datastore
        .find(HarkinnanvarainenHyvaksyminen.class)
        .field("hakuOid")
        .equal(hakuOid)
        .asList();*/
  }

  @Override
  public List<HarkinnanvarainenHyvaksyminen> readByHakuOidAndHakemusOid(
      String hakuOid, String hakemusOid) {
    return null;
/*    return datastore
        .find(HarkinnanvarainenHyvaksyminen.class)
        .field("hakuOid")
        .equal(hakuOid)
        .field("hakemusOid")
        .equal(hakemusOid)
        .asList();*/
  }
}
