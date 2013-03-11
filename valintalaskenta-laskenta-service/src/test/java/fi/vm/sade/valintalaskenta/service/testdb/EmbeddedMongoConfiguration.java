package fi.vm.sade.valintalaskenta.service.testdb;

import java.io.IOException;

import javax.annotation.PreDestroy;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

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
@Configuration
public class EmbeddedMongoConfiguration {

    @Bean
    public MongodExecutable getMongodExecutable(@Value("${mongodb.host}") String host,
            @Value("${mongodb.port}") int port) throws IOException {
        RuntimeConfig runtimeConfig = new RuntimeConfig();
        MongodStarter runtime = MongodStarter.getInstance(runtimeConfig);
        MongodConfig config = new MongodConfig(Version.Main.V2_3, new Net(host, port, false), new Storage(),
                new Timeout());

        MongodExecutable mongodExe = runtime.prepare(config);
        mongodExe.start();
        return mongodExe;
    }

    @Autowired
    MongodExecutable mongodExe;

    @PreDestroy
    public void destroy() {
        mongodExe.stop();
    }
}
