package fi.vm.sade.service.valintaperusteet.algoritmi.domain;

import java.io.IOException;

import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.AnnotationConfigContextLoader;

import com.google.code.morphia.Datastore;
import com.google.code.morphia.Morphia;
import com.mongodb.Mongo;

import de.flapdoodle.embed.mongo.MongodExecutable;
import de.flapdoodle.embed.mongo.MongodStarter;
import de.flapdoodle.embed.mongo.config.AbstractMongoConfig.Net;
import de.flapdoodle.embed.mongo.config.AbstractMongoConfig.Storage;
import de.flapdoodle.embed.mongo.config.AbstractMongoConfig.Timeout;
import de.flapdoodle.embed.mongo.config.MongodConfig;
import de.flapdoodle.embed.mongo.config.RuntimeConfig;
import de.flapdoodle.embed.mongo.distribution.Version;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Ignore
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(loader = AnnotationConfigContextLoader.class)
public class AlgoritmiDomainTest {

    @Configuration
    static class ContextConfiguration {
        private static final int PORT = 37123;
        private static final String HOST = "localhost";

        @Bean
        public Mongo mongo() {
            try {
                RuntimeConfig runtimeConfig = new RuntimeConfig();
                MongodStarter runtime = MongodStarter.getInstance(runtimeConfig);
                MongodConfig config = new MongodConfig(Version.Main.V2_3, new Net(HOST, PORT, false), new Storage(),
                        new Timeout());

                MongodExecutable mongodExe = runtime.prepare(config); // new
                                                                      // MongodConfig(Version.Main.V2_3,
                                                                      // PORT,
                                                                      // false));
                mongodExe.start();
                return new Mongo(HOST, PORT);
            } catch (IOException e) {
                e.printStackTrace();
            }
            return null;
        }

        @Bean
        public Datastore orderService() {
            Morphia morphia = new Morphia();

            return morphia.createDatastore(mongo(), "flapdoodletestdb");
        }
    }

    @Autowired
    Datastore datastore;

    /*
     * @Test public void testAlgoritmiDomain() throws UnknownHostException {
     * Tulos tulos = new Tulos();
     * tulos.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTY); Hakemus hakemus
     * = new Hakemus(); hakemus.getTulokset().add(tulos);
     * datastore.save(hakemus); try { Thread.sleep(TimeUnit.DAYS.toMillis(1)); }
     * catch (InterruptedException e) { // TODO Auto-generated catch block
     * e.printStackTrace(); } }
     */
}
