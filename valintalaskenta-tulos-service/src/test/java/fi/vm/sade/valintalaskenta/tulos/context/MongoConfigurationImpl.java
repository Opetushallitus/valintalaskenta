package fi.vm.sade.valintalaskenta.tulos.context;

import com.github.fakemongo.Fongo;
import com.github.fakemongo.junit.FongoRule;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.connection.ServerVersion;
import dev.morphia.Datastore;
import dev.morphia.Morphia;
import org.junit.Rule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/** @author Jussi Jartamo */
@Configuration
public class MongoConfigurationImpl {
  @Rule public FongoRule fongoRule = new FongoRule(new ServerVersion(3, 6));
  public static final String DATABASE_NAME = "test";

  @Bean
  public Fongo mongoExecutable() {
    return new Fongo("mongo server 1");
  }

  @Bean
  public MongoClient mongoClient(final Fongo mongo) {
    return MongoClients.create(mongo.getServerAddress().toString());
  }

  @Bean(name = "datastore2")
  public Datastore getDatastore(MongoClient mongo) {
    return Morphia.createDatastore(mongo, DATABASE_NAME);
  }
}
