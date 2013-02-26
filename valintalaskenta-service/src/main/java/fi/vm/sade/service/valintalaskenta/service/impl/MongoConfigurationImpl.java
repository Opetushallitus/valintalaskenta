package fi.vm.sade.service.valintalaskenta.service.impl;

import java.net.UnknownHostException;

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

    @Bean
    public Mongo mongo(@Value("${mongodb.host}") String host, @Value("${mongodb.port}") int port)
            throws UnknownHostException {
        return new Mongo(host, port);
    }

    @Bean
    public Datastore createEmbeddedDatastore(@Value("${mongodb.dbname:valintalaskentadb}") String dbname, Mongo mongo) {
        Morphia morphia = new Morphia();
        return morphia.createDatastore(mongo, dbname);
    }

}
