package fi.vm.sade.valintalaskenta.app;

import static fi.vm.sade.valintalaskenta.app.GenerateTestData.getNumberBetween;
import static fi.vm.sade.valintalaskenta.app.GenerateTestData.random;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.config.PropertyPlaceholderConfigurer;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.core.io.ClassPathResource;

import scala.actors.threadpool.Arrays;

import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.valintalaskenta.dto.HakukohdeDto;
import fi.vm.sade.valintalaskenta.service.impl.MongoConfigurationImpl;
import fi.vm.sade.valintalaskenta.service.util.ValintalaskentaServiceUtil;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Configuration
@ImportResource({ "classpath:test-generator-context.xml" })
public class GenerateTestDataToTestDatabase {

    private static final String HOST = "taulu.hard.ware.fi";
    private static final Integer PORT = 27017;

    @Bean
    public PropertyPlaceholderConfigurer getPropertyPlaceholderConfigurer() {
        PropertyPlaceholderConfigurer p = new PropertyPlaceholderConfigurer();
        Properties mongoProps = new Properties();
        mongoProps.setProperty("mongodb.host", HOST);
        mongoProps.setProperty("mongodb.port", PORT.toString());
        p.setProperties(mongoProps);
        return p;
    }

    private static String getStringDoubleValue() {
        return "" + new Double(getNumberBetween(4, 10));
    }

    private static String[] getAvainArvoPari(String avain) {
        return new String[] { avain, getStringDoubleValue() };
    }

    private static final String[] AVAIMET = new String[] { "matematiikka", "aidinkieli", "kuvaamataito", "puukäsityö",
            "psykologia", "maantieto", "biologia", "filosofia", "historia" };

    private static Set<String> getAvaimia() {
        SortedSet<String> avaimia = new TreeSet<String>();
        for (int x = 0; x < 6; ++x) {
            avaimia.add(AVAIMET[random.nextInt(AVAIMET.length) % AVAIMET.length]);
        }
        return avaimia;
    }

    public static void main(String[] args) throws JsonSyntaxException, IOException {
        HakukohdeDto[] hakukohteet = new Gson().fromJson(
                IOUtils.toString(new ClassPathResource("hakukohde.json").getInputStream()), HakukohdeDto[].class);

        ValintalaskentaService valintalaskentaService = new AnnotationConfigApplicationContext(
                GenerateTestDataToTestDatabase.class, MongoConfigurationImpl.class)
                .getBean(ValintalaskentaService.class);

        Integer jarjestysluku = 1;
        for (int i = 0; i < 10; ++i) {

            String hakukohdeoid = hakukohteet[i % hakukohteet.length].getOid();
            String hakemusoid = "hakemusoid-" + new String(GenerateTestData.getRandomChars(6));
            String valinnanvaiheoid = "valinnanvaiheoid-" + new String(GenerateTestData.getRandomChars(6));
            // arvotaan hakijalle satunnaisesti avainarvopareja
            List<String[]> avainparit = new ArrayList<String[]>();
            for (String avain : getAvaimia()) {
                avainparit.add(getAvainArvoPari(avain));
            }
            HakemusTyyppi[] hakemukset = new HakemusTyyppi[] { ValintalaskentaServiceUtil
                    .createHakemusParillaDynaamisesti(hakemusoid, hakukohdeoid, avainparit) };

            ValintaperusteetTyyppi valintaperusteet = ValintalaskentaServiceUtil.createValintaperusteet(hakukohdeoid,
                    valinnanvaiheoid, jarjestysluku);
            for (int a = 0; a < random.nextInt(5) + 1; ++a) {

                ValintatapajonoJarjestyskriteereillaTyyppi jono = ValintalaskentaServiceUtil
                        .createValintatapajono("jonooid-" + new String(GenerateTestData.getRandomChars(6)));
                jono.getJarjestyskriteerit().add(ValintalaskentaServiceUtil.createJarjestyskriteeri());

                // tehdään summafunktio satunnaisilla avainarvopareilla
                // tarkoitus on että satunnaisesti joukko hakijoita tulee
                // epäonnistumaan avainarvoparin puutteessa
                List<String> avaimia = Lists.newArrayList(getAvaimia());
                while (avaimia.size() > 2) {
                    avaimia.remove(random.nextInt(avaimia.size()) % avaimia.size());
                }
                jono.getJarjestyskriteerit().get(0)
                        .setFunktiokutsu(ValintalaskentaServiceUtil.createSummaFunktio(avaimia.get(0), avaimia.get(1)));

                valintaperusteet.getValintatapajonot().add(jono);
            }
            valintalaskentaService.laske(Arrays.asList(hakemukset),
                    Arrays.asList(new ValintaperusteetTyyppi[] { valintaperusteet }));
        }

    }

}
