package fi.vm.sade.valintalaskenta.laskenta.config;

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import dev.morphia.Datastore;
import dev.morphia.Morphia;
import javax.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * <bean id="mongoUri" class="com.mongodb.MongoClientURI"> <constructor-arg type="java.lang.String"
 * value="${valintalaskenta-laskenta-service.mongodb.uri}" /> </bean> <bean id="mongo"
 * class="com.mongodb.MongoClient" scope="singleton"> <constructor-arg
 * type="com.mongodb.MongoClientURI" ref="mongoUri"/> </bean> <bean id="morphia"
 * class="org.mongodb.morphia.Morphia" /> <bean id="datastore2" factory-bean="morphia"
 * factory-method="createDatastore"> <constructor-arg type="com.mongodb.MongoClient" ref="mongo" />
 * <constructor-arg type="java.lang.String"
 * value="${valintalaskenta-laskenta-service.mongodb.dbname}" /> </bean>
 */
@Configuration
public class MongoConfiguration {
  @Bean(name = "mongo")
  public MongoClient getMongoClient(
      @Value("${valintalaskenta-laskenta-service.mongodb.uri}") String uri) {
    return MongoClients.create(uri);
  }

  @Bean(name = "datastore2")
  public Datastore getDatastore(
      MongoClient mongo,
      @Value("${valintalaskenta-laskenta-service.mongodb.dbname}") String dbname) {
    return Morphia.createDatastore(mongo, dbname);
  }

  @PostConstruct
  @Autowired
  public void ensureIndexes(@Qualifier("datastore2") Datastore datastore) {
    datastore.ensureIndexes();
  }
}
