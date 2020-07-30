package fi.vm.sade.valintalaskenta.tulos.context;

import com.mongodb.MongoClient;
import com.mongodb.ServerAddress;
import de.flapdoodle.embed.mongo.MongodExecutable;
import de.flapdoodle.embed.mongo.MongodProcess;
import de.flapdoodle.embed.mongo.MongodStarter;
import de.flapdoodle.embed.mongo.config.IMongodConfig;
import de.flapdoodle.embed.mongo.config.MongodConfigBuilder;
import de.flapdoodle.embed.mongo.config.Net;
import de.flapdoodle.embed.mongo.distribution.Version;
import de.flapdoodle.embed.process.runtime.Network;
import java.io.IOException;
import java.util.Random;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Morphia;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/** @author Jussi Jartamo */
@Configuration
public class MongoConfigurationImpl {

  private static final Logger LOG = LoggerFactory.getLogger(MongoConfigurationImpl.class);

  public static final String DATABASE_NAME = "test";

  static final int PORT = freePort();

  private static int freePort() {
    for (int i = 0; i < 10; ++i) {
      try {
        return Network.getFreeServerPort();
      } catch (IOException e) {
      }
    }
    return 32452 - new Random().nextInt(20000);
  }

  // fake mongo db
  @Bean(destroyMethod = "stop")
  public MongodExecutable getMongodExecutable() throws IOException {
    IMongodConfig mongodConfig =
        new MongodConfigBuilder()
            .version(Version.Main.PRODUCTION)
            .net(new Net(PORT, Network.localhostIsIPv6()))
            .build();

    MongodStarter runtime = MongodStarter.getDefaultInstance();

    MongodExecutable mongodExecutable = null;
    mongodExecutable = runtime.prepare(mongodConfig);

    return mongodExecutable; // .newMongo();
  }

  @Bean(destroyMethod = "stop")
  public MongodProcess getMongoProcess(MongodExecutable mongodExecutable) throws IOException {
    return mongodExecutable.start();
  }

  @Bean
  public MongoClient getMongo(MongodProcess process) throws IOException {

    // MongoClient mongo =
    // return new MongoClient(new ServerAddress(Network.getLocalHost(),
    // Network.getFreeServerPort()));
    return new MongoClient(new ServerAddress("127.0.0.1", PORT)); // factory.newMongo();
  }

  @Bean(name = "morphia2")
  public Morphia getMorphia() {
    return new Morphia();
  }

  @Bean(name = "datastore2")
  public Datastore getDatastore(Morphia morphia, MongoClient mongo) {
    return morphia.createDatastore(mongo, DATABASE_NAME);
  }
}
