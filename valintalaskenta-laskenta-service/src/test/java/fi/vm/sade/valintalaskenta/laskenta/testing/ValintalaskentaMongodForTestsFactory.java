package fi.vm.sade.valintalaskenta.laskenta.testing;

import com.mongodb.MongoClient;
import de.flapdoodle.embed.mongo.config.ImmutableNet;
import de.flapdoodle.embed.mongo.config.Net;
import de.flapdoodle.embed.mongo.distribution.Version;
import de.flapdoodle.embed.mongo.transitions.Mongod;
import de.flapdoodle.embed.mongo.transitions.RunningMongodProcess;
import de.flapdoodle.reverse.TransitionWalker;
import de.flapdoodle.reverse.transitions.Start;
import fi.vm.sade.integrationtest.util.PortChecker;

/**
 * A kludge to work around the problem that the mongo client tries to access mongodb with an
 * external interface, but the testing mongo only binds to localhost.
 */
public class ValintalaskentaMongodForTestsFactory {
  private final TransitionWalker.ReachedState<RunningMongodProcess> mongoProcess;

  public ValintalaskentaMongodForTestsFactory() {
    final Net net = ImmutableNet.defaults().withPort(PortChecker.findFreeLocalPort());
    mongoProcess =
        Mongod.builder()
            .net(Start.to(Net.class).initializedWith(net))
            .build()
            .start(Version.Main.V3_6);
  }

  public void shutdown() {
    if (mongoProcess != null
        && mongoProcess.current() != null
        && mongoProcess.current().isAlive()) {
      System.out.println("Pysäytetään Mongo...");
      mongoProcess.current().stop();
      System.out.println("Mongo pysäytetty");
    }
  }

  public MongoClient newMongo() {
    return new MongoClient("127.0.0.1", mongoProcess.current().getServerAddress().getPort());
  }
}
