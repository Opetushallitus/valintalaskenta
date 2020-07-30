package fi.vm.sade.valintalaskenta.laskenta.testing;

import com.mongodb.MongoClient;
import com.mongodb.ServerAddress;
import de.flapdoodle.embed.mongo.MongodProcess;
import de.flapdoodle.embed.mongo.distribution.Version;
import de.flapdoodle.embed.mongo.tests.MongodForTestsFactory;
import java.io.IOException;
import java.lang.reflect.Field;
import org.springframework.util.ReflectionUtils;

/**
 * A kludge to work around the problem that the mongo client tries to access mongodb with an
 * external interface, but the testing mongo only binds to localhost.
 */
public class ValintalaskentaMongodForTestsFactory extends MongodForTestsFactory {
  public ValintalaskentaMongodForTestsFactory() throws IOException {
    super(Version.V3_4_15);
  }

  @Override
  public MongoClient newMongo() {
    Field mongoDprocessField =
        ReflectionUtils.findField(MongodForTestsFactory.class, "mongodProcess");
    mongoDprocessField.setAccessible(true);
    MongodProcess mongodProcess =
        (MongodProcess) ReflectionUtils.getField(mongoDprocessField, this);
    return new MongoClient(
        new ServerAddress("127.0.0.1", mongodProcess.getConfig().net().getPort()));
  }
}
