package fi.vm.sade.valintalaskenta.laskenta.testing;

import com.github.fakemongo.Fongo;
import com.github.fakemongo.junit.FongoRule;
import com.mongodb.MongoClient;
import com.mongodb.connection.ServerVersion;
import org.junit.Rule;

/**
 * A kludge to work around the problem that the mongo client tries to access mongodb with an
 * external interface, but the testing mongo only binds to localhost.
 */
public class ValintalaskentaMongodForTestsFactory {
  @Rule public FongoRule fongoRule = new FongoRule(new ServerVersion(3, 6));

  private final Fongo mongo;

  public ValintalaskentaMongodForTestsFactory() {
    mongo = new Fongo("mongo server 2");
  }

  public MongoClient newMongo() {
    return mongo.getMongo();
  }
}
