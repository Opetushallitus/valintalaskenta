package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static com.lordofthejars.nosqlunit.core.LoadStrategyEnum.CLEAN_INSERT;
import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializer;
import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import com.mongodb.MongoClientURI;

import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import org.apache.commons.lang.StringUtils;
import org.bson.types.ObjectId;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mongodb.morphia.annotations.Entity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import javax.inject.Named;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * A little utility for dumping valintalaskentadb contents to a form that can be easily used for test fixtures.
 * Add valintalaskentadb URL to run configuration VM parameters, e.g.
 *
 *      <code>-ea -Xmx7G -DVIRKAILIJAMONGO_URL=mongodb://oph:PASSWORD@qa-mongodb1.oph.ware.fi,qa-mongodb2.oph.ware.fi,qa-mongodb3.oph.ware.fi:27017</code>
 *
 * Note that the JSONs produced still need some manual fixing. At least
 *   * "id" fields of entities need to be changed to "_id"
 *   * Valintatapajono sub-entities in Valinnanvaihe entities need to be changed to references (see existing fixtures for details)
 */
@ContextConfiguration(locations = ValintalaskentaDbDumper.SPRING_CONFIG_XML)
@RunWith(SpringJUnit4ClassRunner.class)
@UsingDataSet
public class ValintalaskentaDbDumper {
    private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaDbDumper.class);
    protected static final String SPRING_CONFIG_XML = "classpath:fi/vm/sade/valintalaskenta/laskenta/service/it/test-valintalaskentadb-access.xml";

    private final Gson gson;

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    @Named("mongoUri2")
    private MongoClientURI mongoClientURI;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    private static final String MONGO_SYSTEM_PROPERTY_NAME = "VIRKAILIJAMONGO_URL";
    private static String originalMongoSystemProperty;

    /**
     * The real method used for dumping the data
     */
    public static void main(String... args) {
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext(SPRING_CONFIG_XML);
        ValintalaskentaDbDumper dumper = (ValintalaskentaDbDumper) context.getBean("valintalaskentaDbDumper");
        OidsToDump oidsToDump = OidsToDump.withHakuOid("1.2.246.562.29.59856749474")
            .withHakukohdeOids("1.2.246.562.20.68517235666", "1.2.246.562.20.80972757381")
            .withHakemusOids("1.2.246.562.11.00009176948").build();
        dumper.dumpData(oidsToDump);
    }

    public ValintalaskentaDbDumper() {
        JsonSerializer<ObjectId> objectIdJsonSerializer = (src, typeOfSrc, context) -> {
            JsonObject jsonObject = new JsonObject();
            jsonObject.add("$oid", new JsonPrimitive(src.toHexString()));
            return jsonObject;
        };
        gson = new GsonBuilder().registerTypeAdapter(ObjectId.class, objectIdJsonSerializer).create();
    }

    @BeforeClass
    public static void setMongoSystemPropertyForAccessingTestValintaLaskentaDb() {
        String propertyBefore = System.getProperty(MONGO_SYSTEM_PROPERTY_NAME);
        if (StringUtils.isNotBlank(propertyBefore)) {
            originalMongoSystemProperty = propertyBefore;
        }
        System.setProperty(MONGO_SYSTEM_PROPERTY_NAME, "mongodb://localhost");
    }

    @AfterClass
    public static void clearMongoSystemProperty() {
        if (originalMongoSystemProperty == null) {
            System.clearProperty(MONGO_SYSTEM_PROPERTY_NAME);
        } else {
            System.setProperty(MONGO_SYSTEM_PROPERTY_NAME, originalMongoSystemProperty);
        }
    }


    @Test
    @UsingDataSet(locations = "voidaanHyvaksya.json", loadStrategy = CLEAN_INSERT)
    public void smokeTest() {
        LOG.info("Using valintalaskentadb from " + mongoClientURI.getHosts());
    }

    public void dumpData(OidsToDump oidsToDump) {
        List<Valinnanvaihe> valinnanvaihes = new LinkedList<>();
        for (int jarjestysNumero = 0; jarjestysNumero < 10; jarjestysNumero++) {
            for (String hakukohdeOid : oidsToDump.hakukohdeOids) {
                valinnanvaihes.addAll(valinnanvaiheDAO.haeValinnanvaiheetJarjestysnumerolla(oidsToDump.hakuOid, hakukohdeOid, jarjestysNumero));
            }
        }
        cleanUpForPrinting(valinnanvaihes, oidsToDump.hakemusOids);

        List<Valintatapajono> jonot = valinnanvaihes.stream().flatMap(vv -> vv.getValintatapajonot().stream()).collect(Collectors.toList());
        List<Jonosija> jonosijat = jonot.stream().flatMap(j -> j.getJonosijat().stream()).collect(Collectors.toList());

        jonot.forEach(j -> j.getJonosijat().clear());

        System.out.println("\"" + collectionName(Jonosija.class) + "\": " + gson.toJson(jonosijat));
        System.out.println("\"" + collectionName(Valintatapajono.class) + "\": " + gson.toJson(jonot));
        System.out.println("\"" + collectionName(Valinnanvaihe.class) + "\": " + gson.toJson(valinnanvaihes));
    }

    private String collectionName(Class<?> clazz) {
        return clazz.getAnnotation(Entity.class).value();
    }

    private void cleanUpForPrinting(List<Valinnanvaihe> valinnanvaihes, List<String> hakemusOids) {
        valinnanvaihes.forEach(vaihe -> {
            vaihe.setCreatedAt(null);
            Set<Valintatapajono> sailytettavatJonot = new HashSet<>();
            vaihe.getValintatapajonot().forEach(jono -> {
                List<Jonosija> sailytettavatJonosijat = new LinkedList<>();
                List<ObjectId> sailytettavatJonosijaIdt = new LinkedList<>();
                jono.getJonosijat().forEach(jonosija -> {
                    if (hakemusOids.contains(jonosija.getHakemusOid())) {
                        sailytettavatJonosijat.add(jonosija);
                        sailytettavatJonot.add(jono);
                    }
                });
                jono.getJonosijaIdt().forEach(id -> {
                    if (sailytettavatJonosijat.stream().anyMatch(sailytettava -> sailytettava.getId().equals(id))) {
                        sailytettavatJonosijaIdt.add(id);
                    }
                });
                jono.setJonosijat(sailytettavatJonosijat);
                jono.setJonosijaIdt(sailytettavatJonosijaIdt);
            });
            vaihe.setValintatapajonot(new ArrayList<>(sailytettavatJonot));
        });
    }

    public static class OidsToDump {
        public final String hakuOid;
        public final List<String> hakukohdeOids;
        public final List<String> hakemusOids;

        public OidsToDump(String hakuOid, List<String> hakukohdeOids, List<String> hakemusOids) {
            this.hakuOid = hakuOid;
            this.hakukohdeOids = hakukohdeOids;
            this.hakemusOids = hakemusOids;
        }

        public static OidsToDump.Builder withHakuOid(String hakuOid) {
            return new OidsToDump.Builder(hakuOid);
        }

        public static class Builder {
            private final String hakuOid;
            private String[] hakukohdeOids;
            private String[] hakemusOids;

            public Builder(String hakuOid) {
                this.hakuOid = hakuOid;
            }

            public Builder withHakukohdeOids(String... hakukohdeOids) {
                this.hakukohdeOids = hakukohdeOids;
                return this;
            }

            public Builder withHakemusOids(String... hakemusOids) {
                this.hakemusOids = hakemusOids;
                return this;
            }

            public OidsToDump build() {
                return new OidsToDump(hakuOid, Arrays.asList(hakukohdeOids), Arrays.asList(hakemusOids));
            }
        }
    }
}
