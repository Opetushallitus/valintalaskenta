package fi.vm.sade.valintalaskenta.laskenta.config;

import com.mongodb.MongoClient;
import com.mongodb.MongoClientURI;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Morphia;
import org.mongodb.morphia.mapping.DefaultCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Profile("!dev")
@Configuration
public class MongoConfiguration {
  private static final Logger LOG = LoggerFactory.getLogger(MongoConfiguration.class);

  @Bean(name = "mongoClient")
  public MongoClient mongoClient(
      @Value("${valintalaskenta-laskenta-service.mongodb.uri}") String uri) {
    LOG.info("Creating bean mongoClient with uri " + uri);
    return new MongoClient(new MongoClientURI(uri));
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
      @Qualifier("morphia") Morphia morphia,
      @Qualifier("mongoClient") final MongoClient mongoClient,
      @Value("${valintalaskenta-laskenta-service.mongodb.dbname}") String dbname) {
    LOG.info("Creating bean datastore2 with dbname " + dbname);
    return morphia.createDatastore(mongoClient, dbname);
  }
}
