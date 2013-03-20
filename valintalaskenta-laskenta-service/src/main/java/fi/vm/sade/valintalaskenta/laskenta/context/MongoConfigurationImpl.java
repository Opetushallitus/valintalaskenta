package fi.vm.sade.valintalaskenta.laskenta.context;

import java.net.UnknownHostException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.google.code.morphia.Datastore;
import com.google.code.morphia.Morphia;
import com.mongodb.Mongo;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Configuration
public class MongoConfigurationImpl {

    private static final Logger LOG = LoggerFactory.getLogger(MongoConfigurationImpl.class);

    @Bean
    public Mongo mongo(@Value("${mongodb.host}") String host, @Value("${mongodb.port}") int port)
            throws UnknownHostException {
        LOG.info("Otetaan yhteys Mongokantaan osoitteessa {}:{}", new Object[] { host, port });
        return new Mongo(host, port);
    }

    @Bean
    public Datastore createEmbeddedDatastore(@Value("${mongodb.dbname:valintalaskentadb}") String dbname, Mongo mongo) {
        Morphia morphia = new Morphia();
        return morphia.createDatastore(mongo, dbname);
    }

}
