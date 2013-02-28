package fi.vm.sade.valintalaskenta.app;

import java.security.SecureRandom;
import java.util.Properties;
import java.util.Random;

import org.springframework.beans.factory.config.PropertyPlaceholderConfigurer;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;

import scala.actors.threadpool.Arrays;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.valintalaskenta.service.impl.MongoConfigurationImpl;
import fi.vm.sade.valintalaskenta.service.util.ValintalaskentaServiceUtil;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Luo satunnaista testidataa paikalliseen sulautettuun mongokantaan.
 *         Sovelluksen saa suorittaa useamman kerran. Joka kerralla luodaan 10
 *         laskentaa.
 * 
 */
@Configuration
@ImportResource({ "classpath:test-generator-context.xml" })
public class GenerateTestData {

    private static final String HOST = "localhost";
    private static final Integer PORT = 37200;

    @Bean
    public PropertyPlaceholderConfigurer getPropertyPlaceholderConfigurer() {
        PropertyPlaceholderConfigurer p = new PropertyPlaceholderConfigurer();
        Properties mongoProps = new Properties();
        mongoProps.setProperty("mongodb.host", HOST);
        mongoProps.setProperty("mongodb.port", PORT.toString());
        p.setProperties(mongoProps);
        return p;
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) {
        ValintalaskentaService valintalaskentaService = new AnnotationConfigApplicationContext(GenerateTestData.class,
                MongoConfigurationImpl.class).getBean(ValintalaskentaService.class);

        Integer jarjestysluku = 1;
        for (int i = 0; i < 10; ++i) {
            String doubleValue = "" + new Double(getNumberBetween(4, 10));

            String hakukohdeoid = "hakukohde-" + new String(getRandomChars(6));
            String hakemusoid = "hakemus-" + new String(getRandomChars(6));
            HakemusTyyppi[] hakemukset = new HakemusTyyppi[] { ValintalaskentaServiceUtil.createHakemusParilla(
                    hakemusoid, hakukohdeoid, new String[] { "matematiikka", doubleValue }, new String[] {
                            "aidinkieli", doubleValue }) };

            ValintaperusteetTyyppi valintaperusteet = ValintalaskentaServiceUtil.createValintaperusteet(hakukohdeoid,
                    jarjestysluku);
            ValintatapajonoJarjestyskriteereillaTyyppi jono = ValintalaskentaServiceUtil
                    .createValintatapajono("jonooid0");
            valintaperusteet.getValintatapajonot().add(jono);
            jono.getJarjestyskriteerit().add(ValintalaskentaServiceUtil.createJarjestyskriteeri());

            jono.getJarjestyskriteerit().get(0)
                    .setFunktiokutsu(ValintalaskentaServiceUtil.createSummaFunktio("matematiikka", "aidinkieli"));
            valintalaskentaService.laske(hakukohdeoid, jarjestysluku, Arrays.asList(hakemukset),
                    Arrays.asList(new ValintaperusteetTyyppi[] { valintaperusteet }));
        }
    }

    private static Random random = new SecureRandom();

    public static int getNumberBetween(int min, int max) {

        if (max < min) {
            throw new IllegalArgumentException(String.format("Minimum must be less than minimum (min=%d, max=%d)", min,
                    max));
        }

        return min + random.nextInt(max - min);
    }

    public static String getRandomChars(int length) {
        return getRandomChars(length, length);
    }

    public static char getRandomChar() {
        return (char) (random.nextInt(26) + 'a');
    }

    public static String getRandomChars(int minLength, int maxLength) {
        validateMinMaxParams(minLength, maxLength);
        StringBuilder sb = new StringBuilder(maxLength);

        int length = minLength;
        if (maxLength != minLength) {
            length = length + random.nextInt(maxLength - minLength);
        }
        while (length > 0) {
            sb.append(getRandomChar());
            length--;
        }
        return sb.toString();
    }

    private static void validateMinMaxParams(int minLength, int maxLength) {
        if (minLength < 0) {
            throw new IllegalArgumentException("Minimum length must be a non-negative number");
        }

        if (maxLength < 0) {
            throw new IllegalArgumentException("Maximum length must be a non-negative number");
        }

        if (maxLength < minLength) {
            throw new IllegalArgumentException(String.format(
                    "Minimum length must be less than maximum length (min=%d, max=%d)", minLength, maxLength));
        }
    }
}
