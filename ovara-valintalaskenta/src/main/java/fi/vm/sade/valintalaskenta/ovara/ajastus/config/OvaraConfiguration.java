package fi.vm.sade.valintalaskenta.ovara.ajastus.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;

@Configuration
@EnableJdbcRepositories(
    basePackages = {
      "fi.vm.sade.valintalaskenta.tulos.dao.repository",
      "fi.vm.sade.valintalaskenta.laskenta.dao.repository"
    })
public class OvaraConfiguration {}
