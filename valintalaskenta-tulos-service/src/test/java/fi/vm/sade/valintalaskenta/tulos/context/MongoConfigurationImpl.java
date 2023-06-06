package fi.vm.sade.valintalaskenta.tulos.context;

import com.mongodb.MongoClient;
import de.flapdoodle.embed.mongo.commands.ServerAddress;
import de.flapdoodle.embed.mongo.distribution.Version;
import de.flapdoodle.embed.mongo.transitions.Mongod;
import de.flapdoodle.embed.mongo.transitions.RunningMongodProcess;
import de.flapdoodle.reverse.TransitionWalker.ReachedState;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Morphia;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/** @author Jussi Jartamo */
@Configuration
public class MongoConfigurationImpl {
  public static final String DATABASE_NAME = "test";

  @Bean(destroyMethod = "close")
  public ReachedState<RunningMongodProcess> mongodProcess() {
    return Mongod.instance().start(Version.Main.V3_6);
  }

  @Bean
  public MongoClient mongoClient(final ReachedState<RunningMongodProcess> mongodProcess) {
    final ServerAddress serverAddress = mongodProcess.current().getServerAddress();
    return new MongoClient(serverAddress.getHost(), serverAddress.getPort());
  }

  @Bean(name = "morphia2")
  public Morphia getMorphia() {
    return new Morphia();
  }

  @Bean(name = "datastore2")
  public Datastore getDatastore(final Morphia morphia, final MongoClient mongo) {
    return morphia.createDatastore(mongo, DATABASE_NAME);
  }
}
