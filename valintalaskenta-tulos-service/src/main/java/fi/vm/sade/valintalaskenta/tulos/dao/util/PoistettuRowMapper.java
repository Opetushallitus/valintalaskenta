package fi.vm.sade.valintalaskenta.tulos.dao.util;

import fi.vm.sade.valintalaskenta.domain.siirtotiedosto.Poistettu;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.UUID;
import org.springframework.jdbc.core.RowMapper;

public class PoistettuRowMapper implements RowMapper<Poistettu> {
  @Override
  public Poistettu mapRow(ResultSet rs, int rowNum) throws SQLException {
    String idStr = rs.getString("id");
    String parentIdStr = rs.getString("parentId");
    return new Poistettu(
        UUID.fromString(idStr),
        parentIdStr != null ? UUID.fromString(parentIdStr) : null,
        rs.getString("tunniste"));
  }
}
