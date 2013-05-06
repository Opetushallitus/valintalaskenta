package fi.vm.sade.valintalaskenta.tulos.context;

import java.net.UnknownHostException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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

    @Autowired
    Mongo mongo;

    @Bean
    public Datastore createEmbeddedDatastore() {
        Morphia morphia = new Morphia();
        return morphia.createDatastore(mongo, "test");
    }

}
