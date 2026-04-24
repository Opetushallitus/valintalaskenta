package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.laskenta.dao.impl.SureHakuOidDaoImpl;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.util.Set;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;

public class SureHakuOidDaoTest extends AbstractIntegrationTest {

  @Autowired private JdbcTemplate jdbcTemplate;

  @BeforeEach
  public void tyhjennaSureHakuOidit() {
    jdbcTemplate.update("DELETE FROM sure_haku_oids");
  }

  @Test
  public void palauttaaTyhjanJoukonKunTaulussaEiRiveja() {
    SureHakuOidDao dao = new SureHakuOidDaoImpl(jdbcTemplate);
    Assertions.assertEquals(Set.of(), dao.haeSureHakuOidit());
  }

  @Test
  public void palauttaaTaulunHakuOidit() {
    jdbcTemplate.update("INSERT INTO sure_haku_oids (haku_oid) VALUES (?)", "1.2.3.4");
    jdbcTemplate.update("INSERT INTO sure_haku_oids (haku_oid) VALUES (?)", "5.6.7.8");

    SureHakuOidDao dao = new SureHakuOidDaoImpl(jdbcTemplate);
    Assertions.assertEquals(Set.of("1.2.3.4", "5.6.7.8"), dao.haeSureHakuOidit());
  }
}
