package fi.vm.sade.valintalaskenta.config;

import static java.util.Arrays.asList;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import fi.vm.sade.valintalaskenta.domain.valinta.FunktioTulosContainer;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteeritulosContainer;
import fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvoContainer;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.TimeUnit;
import javax.sql.DataSource;
import org.postgresql.util.PGobject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.TypeDescriptor;
import org.springframework.core.convert.converter.GenericConverter;
import org.springframework.data.convert.ReadingConverter;
import org.springframework.data.convert.WritingConverter;
import org.springframework.data.jdbc.core.convert.JdbcCustomConversions;
import org.springframework.data.jdbc.repository.config.AbstractJdbcConfiguration;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

@Configuration
@EnableJdbcRepositories(
    basePackages = {
      "fi.vm.sade.valintalaskenta.laskenta.dao.repository",
      "fi.vm.sade.valintalaskenta.tulos.dao.repository"
    })
class DatabaseConfiguration extends AbstractJdbcConfiguration {

  private final ApplicationContext applicationContext;

  private static final List<Class<?>> JSON_CLASSES =
      asList(
          SyotettyArvoContainer.class,
          FunktioTulosContainer.class,
          JarjestyskriteeritulosContainer.class);

  DatabaseConfiguration(ApplicationContext applicationContext) {
    this.applicationContext = applicationContext;
  }

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
  DataSourceTransactionManager transactionManager(DataSource dataSource) {
    return new DataSourceTransactionManager(dataSource);
  }

  @Bean
  TransactionTemplate transactionTemplate(DataSourceTransactionManager transactionManager) {
    return new TransactionTemplate(transactionManager);
  }

  @Override
  @Bean
  public JdbcCustomConversions jdbcCustomConversions() {
    List<GenericConverter> converters = new ArrayList<>();
    JSON_CLASSES.forEach(
        clazz -> {
          converters.add(new ObjectToJSONB<>(clazz));
          converters.add(new JSONBToObject<>(clazz));
        });
    return new JdbcCustomConversions(converters);
  }

  @WritingConverter
  class ObjectToJSONB<S> implements GenericConverter {

    private final Class<S> sourceClazz;

    private final ObjectMapper mapper;

    ObjectToJSONB(Class<S> sourceClazz) {
      this.sourceClazz = sourceClazz;
      mapper = applicationContext.getBean(ObjectMapper.class);
    }

    @Override
    public Set<ConvertiblePair> getConvertibleTypes() {
      return Collections.singleton(
          new GenericConverter.ConvertiblePair(sourceClazz, PGobject.class));
    }

    @Override
    public Object convert(Object source, TypeDescriptor sourceType, TypeDescriptor targetType) {
      Object sourceObject = sourceClazz.cast(source);
      PGobject pgJsonObject = new PGobject();
      pgJsonObject.setType("jsonb");
      try {
        pgJsonObject.setValue(mapper.writeValueAsString(sourceObject));
      } catch (SQLException | JsonProcessingException e) {
        throw new RuntimeException(e);
      }
      return pgJsonObject;
    }
  }

  @ReadingConverter
  class JSONBToObject<S> implements GenericConverter {
    private final Class<S> targetClazz;

    private final ObjectMapper mapper;

    JSONBToObject(Class<S> targetClazz) {
      this.targetClazz = targetClazz;
      mapper = applicationContext.getBean(ObjectMapper.class);
    }

    @Override
    public Set<ConvertiblePair> getConvertibleTypes() {
      return Collections.singleton(
          new GenericConverter.ConvertiblePair(PGobject.class, targetClazz));
    }

    @Override
    public Object convert(Object source, TypeDescriptor sourceType, TypeDescriptor targetType) {
      PGobject pgObject = (PGobject) source;
      try {
        return mapper.readValue(pgObject.getValue(), targetClazz);
      } catch (JsonProcessingException e) {
        throw new RuntimeException(e);
      }
    }
  }
}
