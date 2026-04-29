package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import fi.vm.sade.valintalaskenta.laskenta.dao.SureHakuOidDao;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional
public class SureHakuOidDaoImpl implements SureHakuOidDao {

  private final JdbcTemplate jdbcTemplate;
  private final Supplier<Set<String>> cache =
      Suppliers.memoizeWithExpiration(this::haeTietokannasta, 1, TimeUnit.MINUTES);

  @Autowired
  public SureHakuOidDaoImpl(JdbcTemplate jdbcTemplate) {
    this.jdbcTemplate = jdbcTemplate;
  }

  @Override
  public Set<String> haeSureHakuOidit() {
    return cache.get();
  }

  private Set<String> haeTietokannasta() {
    return new HashSet<>(
        this.jdbcTemplate.queryForList("SELECT haku_oid FROM sure_haku_oids", String.class));
  }
}
