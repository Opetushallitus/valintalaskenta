package fi.vm.sade.valintalaskenta.laskenta.config;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientURI;
import java.io.IOException;
import java.net.UnknownHostException;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Morphia;
import org.mongodb.morphia.mapping.DefaultCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

/**
 * <bean id="mongoUri" class="com.mongodb.MongoClientURI"> <constructor-arg type="java.lang.String"
 * value="${valintalaskenta-laskenta-service.mongodb.uri}" /> </bean> <bean id="mongo"
 * class="com.mongodb.MongoClient" scope="singleton"> <constructor-arg
 * type="com.mongodb.MongoClientURI" ref="mongoUri"/> </bean> <bean id="morphia"
 * class="org.mongodb.morphia.Morphia" /> <bean id="datastore2" factory-bean="morphia"
 * factory-method="createDatastore"> <constructor-arg type="com.mongodb.MongoClient" ref="mongo" />
 * <constructor-arg type="java.lang.String"
 * value="${valintalaskenta-laskenta-service.mongodb.dbname}" /> </bean>
 */
@Profile("!dev")
@Configuration
public class MongoConfiguration {
  private static final Logger LOG = LoggerFactory.getLogger(MongoConfiguration.class);

  public MongoConfiguration(
      @Value("${valintalaskenta-laskenta-service.mongodb.uri}") final String uri,
      @Value("${valintalaskenta-laskenta-service.mongodb.dbname}") String dbname) {
    LOG.info("Mongo uri: " + uri);
    LOG.info("Mongo dbname: " + dbname);
  }

  @Bean(name = "mongoUri")
  public MongoClientURI getMongoUri(
      @Value("${valintalaskenta-laskenta-service.mongodb.uri}") String uri) throws IOException {
    return new MongoClientURI(uri);
  }

  @Bean(name = "mongo")
  public MongoClient getMongoClient(MongoClientURI clientUri) throws UnknownHostException {
    return new MongoClient(clientUri);
  }

  @Bean(name = "morphia")
  public Morphia getMorphia() {
    Morphia morphia = new Morphia();
    morphia
        .getMapper()
        .getOptions()
        .setObjectFactory(
            new DefaultCreator() {
              @Override
              protected ClassLoader getClassLoaderForClass() {
                return MongoConfiguration.class.getClassLoader();
              }
            });
    return morphia;
  }

  @Bean(name = "datastore2")
  public Datastore getDatastore(
      Morphia morphia,
      MongoClient mongo,
      @Value("${valintalaskenta-laskenta-service.mongodb.dbname}") String dbname) {
    return morphia.createDatastore(mongo, dbname);
  }
}
