package fi.vm.sade.valintalaskenta.ovara.ajastus.config;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import java.util.*;
import java.util.concurrent.TimeUnit;
import javax.sql.DataSource;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.jdbc.repository.config.AbstractJdbcConfiguration;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.TransactionManager;

@Configuration
@Profile("ovara")
class OvaraDatabaseConfiguration extends AbstractJdbcConfiguration {

  OvaraDatabaseConfiguration() {}

  @Bean
  public DataSource dataSource(
      @Value("${valintalaskenta-laskenta-service.postgresql.maxactive}") final String maxPoolSize,
      @Value("${valintalaskenta-laskenta-service.postgresql.maxwait}") final String maxWait,
      @Value("${valintalaskenta-laskenta-service.postgresql.leakdetectionthresholdmillis}")
          final String leaksThreshold,
      @Value("${valintalaskenta-laskenta-service.postgresql.url}") final String url,
      @Value("${valintalaskenta-laskenta-service.postgresql.user}") final String user,
      @Value("${valintalaskenta-laskenta-service.postgresql.password}") final String password,
      @Value("${valintalaskenta-laskenta-service.postgresql.driver}") final String driverClassName,
      @Value("${valintalaskenta-laskenta-service.postgresql.idletimeoutminutes:10}")
          final String idleTimeout,
      @Value("${valintalaskenta-laskenta-service.postgresql.minidle:0}") final String minIdle) {
    final HikariConfig config = new HikariConfig();
    config.setPoolName("springHikariCP");
    config.setConnectionTestQuery("SELECT 1");
    config.setJdbcUrl(url);
    config.setDriverClassName(driverClassName);
    config.setMaximumPoolSize(Integer.parseInt(maxPoolSize));
    config.setMaxLifetime(Long.parseLong(maxWait));
    config.setLeakDetectionThreshold(Long.parseLong(leaksThreshold));
    config.setIdleTimeout(TimeUnit.MINUTES.toMillis(Long.parseLong(idleTimeout)));
    config.setMinimumIdle(Integer.parseInt(minIdle));
    config.setRegisterMbeans(false);
    final Properties dsProperties = new Properties();
    dsProperties.setProperty("url", url);
    dsProperties.setProperty("user", user);
    dsProperties.setProperty("password", password);
    config.setDataSourceProperties(dsProperties);
    return new HikariDataSource(config);
  }

  @Bean
  NamedParameterJdbcOperations namedParameterJdbcOperations(DataSource dataSource) {
    return new NamedParameterJdbcTemplate(dataSource);
  }

  @Bean
  TransactionManager transactionManager(DataSource dataSource) {
    return new DataSourceTransactionManager(dataSource);
  }
}
