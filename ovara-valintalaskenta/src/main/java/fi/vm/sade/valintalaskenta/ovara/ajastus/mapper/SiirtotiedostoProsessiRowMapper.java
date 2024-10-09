package fi.vm.sade.valintalaskenta.ovara.ajastus.mapper;

import fi.vm.sade.valintalaskenta.ovara.ajastus.SiirtotiedostoProsessi;
import java.sql.ResultSet;
import java.sql.SQLException;
import org.springframework.jdbc.core.RowMapper;

public class SiirtotiedostoProsessiRowMapper implements RowMapper<SiirtotiedostoProsessi> {
  @Override
  public SiirtotiedostoProsessi mapRow(ResultSet rs, int rowNum) throws SQLException {
    return new SiirtotiedostoProsessi(
        rs.getString("execution_uuid"),
        rs.getTimestamp("window_start"),
        rs.getTimestamp("window_end"),
        rs.getTimestamp("run_start"),
        rs.getTimestamp("run_end"),
        rs.getString("info"),
        rs.getBoolean("success"),
        rs.getString("error_message"));
  }
}
