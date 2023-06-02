package fi.vm.sade.valintalaskenta.laskenta.testing;

import com.mongodb.MongoClient;
import de.flapdoodle.embed.mongo.commands.ServerAddress;
import de.flapdoodle.embed.mongo.distribution.Version;
import de.flapdoodle.embed.mongo.transitions.Mongod;
import de.flapdoodle.embed.mongo.transitions.RunningMongodProcess;
import de.flapdoodle.reverse.TransitionWalker;

public class ValintalaskentaMongodForTestsFactory {
  private final TransitionWalker.ReachedState<RunningMongodProcess> mongodProcess;

  public ValintalaskentaMongodForTestsFactory() {
    mongodProcess = new Mongod().start(Version.Main.V3_6);
  }

  public void shutdown() {
    if (mongodProcess != null
        && mongodProcess.current() != null
        && mongodProcess.current().isAlive()) {
      int port = mongodProcess.current().getServerAddress().getPort();
      System.out.println("Pysäytetään mongod (" + port + ")...");
      mongodProcess.current().stop();
      System.out.println("Mongod pysäytetty (" + port + ")");
    }
  }

  public MongoClient newMongo() {
    final ServerAddress serverAddress = mongodProcess.current().getServerAddress();
    return new MongoClient(serverAddress.getHost(), serverAddress.getPort());
  }
}
