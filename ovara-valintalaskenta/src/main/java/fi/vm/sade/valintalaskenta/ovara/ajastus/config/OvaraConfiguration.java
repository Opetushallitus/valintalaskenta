package fi.vm.sade.valintalaskenta.ovara.ajastus.config;

import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogImpl;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;

@Configuration
@EnableJdbcRepositories(
    basePackages = {
      "fi.vm.sade.valintalaskenta.tulos.dao.repository",
      "fi.vm.sade.valintalaskenta.laskenta.dao.repository"
    })
public class OvaraConfiguration {

  @Bean(name = "modelMapper")
  public ValintalaskentaModelMapper modelMapper() {
    return new ValintalaskentaModelMapper();
  }

  @Bean
  public LaskentaAudit laskentaAudit() {
    return new LaskentaAudit();
  }

  @Bean
  public LaskentaAuditLogImpl LaskentaAuditLog() {
    return new LaskentaAuditLogImpl();
  }
}
