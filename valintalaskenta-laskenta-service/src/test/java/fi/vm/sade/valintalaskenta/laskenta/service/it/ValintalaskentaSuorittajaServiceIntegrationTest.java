package fi.vm.sade.valintalaskenta.laskenta.service.it;

import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.valintaperusteet.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.SyoteparametriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.TavallinenValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * User: wuoti
 * Date: 5.9.2013
 * Time: 12.17
 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(listeners = {DependencyInjectionTestExecutionListener.class,
        DirtiesContextTestExecutionListener.class})
public class ValintalaskentaSuorittajaServiceIntegrationTest {
    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private ValintalaskentaSuorittajaService valintalaskentaSuorittajaService;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

    private static FunktiokutsuTyyppi sata;
    private static FunktiokutsuTyyppi kaksisataa;
    private static FunktiokutsuTyyppi kolmesataa;
    private static FunktiokutsuTyyppi neljasataa;
    private static FunktiokutsuTyyppi viisisataa;
    private static FunktiokutsuTyyppi kuusisataa;
    private static FunktiokutsuTyyppi seitsemansataa;
    private static FunktiokutsuTyyppi kahdeksansataa;


    static {
        sata = new FunktiokutsuTyyppi();
        sata.setFunktionimi(Funktionimi.LUKUARVO.name());
        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
            param.setAvain("luku");
            param.setArvo("100.0");
            sata.getSyoteparametrit().add(param);
        }

        kaksisataa = new FunktiokutsuTyyppi();
        kaksisataa.setFunktionimi(Funktionimi.LUKUARVO.name());
        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
            param.setAvain("luku");
            param.setArvo("200.0");
            kaksisataa.getSyoteparametrit().add(param);
        }

        kolmesataa = new FunktiokutsuTyyppi();
        kolmesataa.setFunktionimi(Funktionimi.LUKUARVO.name());
        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
            param.setAvain("luku");
            param.setArvo("300.0");
            kolmesataa.getSyoteparametrit().add(param);
        }

        neljasataa = new FunktiokutsuTyyppi();
        neljasataa.setFunktionimi(Funktionimi.LUKUARVO.name());
        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
            param.setAvain("luku");
            param.setArvo("400.0");
            neljasataa.getSyoteparametrit().add(param);
        }

        viisisataa = new FunktiokutsuTyyppi();
        viisisataa.setFunktionimi(Funktionimi.LUKUARVO.name());
        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
            param.setAvain("luku");
            param.setArvo("500.0");
            viisisataa.getSyoteparametrit().add(param);
        }

        kuusisataa = new FunktiokutsuTyyppi();
        kuusisataa.setFunktionimi(Funktionimi.LUKUARVO.name());
        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
            param.setAvain("luku");
            param.setArvo("600.0");
            kuusisataa.getSyoteparametrit().add(param);
        }

        seitsemansataa = new FunktiokutsuTyyppi();
        seitsemansataa.setFunktionimi(Funktionimi.LUKUARVO.name());
        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
            param.setAvain("luku");
            param.setArvo("700.0");
            seitsemansataa.getSyoteparametrit().add(param);
        }

        kahdeksansataa = new FunktiokutsuTyyppi();
        kahdeksansataa.setFunktionimi(Funktionimi.LUKUARVO.name());
        {
            SyoteparametriTyyppi param = new SyoteparametriTyyppi();
            param.setAvain("luku");
            param.setArvo("800.0");
            kahdeksansataa.getSyoteparametrit().add(param);
        }
    }


    @Test
    public void test() {
        final String hakuOid = "hakuOid1";
        final String hakukohdeOid1 = "hakukohdeOid1";
        final String hakukohdeOid2 = "hakukohdeOid2";
        final String valinnanvaiheOid1 = "valinnanvaiheOid1";
        final String valinnanvaiheOid2 = "valinnanvaiheOid2";

        final String valinnanvaiheOid3 = "valinnanvaiheOid3";
        final String valinnanvaiheOid4 = "valinnanvaiheOid4";

        final String valintatapajonoOid1 = "valintatapajonoOid1";
        final String valintatapajonoOid2 = "valintatapajonoOid2";
        final String valintatapajonoOid3 = "valintatapajonoOid3";
        final String valintatapajonoOid4 = "valintatapajonoOid4";
        final String valintatapajonoOid5 = "valintatapajonoOid5";
        final String valintatapajonoOid6 = "valintatapajonoOid6";

        final String hakemusOid1 = "hakemusOid1";
        final String hakemusOid2 = "hakemusOid2";

        final String hakijaOid1 = "hakijaOid1";
        final String hakijaOid2 = "hakijaOid2";
        {
            ValintaperusteetTyyppi valintaperusteet1 = luoValintaperusteet(hakuOid, hakukohdeOid1);
            TavallinenValinnanVaiheTyyppi valinnanvaihe1 = luoTavallinenValinnanvaihe(valinnanvaiheOid1, 0);
            valinnanvaihe1.getValintatapajono().add(luoValintatapajono(valintatapajonoOid1, 1, 10, luoJarjestyskriteeri(sata, 1), luoJarjestyskriteeri(kaksisataa, 2)));
            valinnanvaihe1.getValintatapajono().add(luoValintatapajono(valintatapajonoOid2, 2, 20, luoJarjestyskriteeri(kolmesataa, 1)));
            valintaperusteet1.setValinnanVaihe(valinnanvaihe1);

            ValintaperusteetTyyppi valintaperusteet2 = luoValintaperusteet(hakuOid, hakukohdeOid1);
            TavallinenValinnanVaiheTyyppi valinnanvaihe2 = luoTavallinenValinnanvaihe(valinnanvaiheOid2, 1);
            valinnanvaihe2.getValintatapajono().add(luoValintatapajono(valintatapajonoOid3, 1, 30, luoJarjestyskriteeri(neljasataa, 1)));
            valintaperusteet2.setValinnanVaihe(valinnanvaihe2);

            ValintaperusteetTyyppi valintaperusteet3 = luoValintaperusteet(hakuOid, hakukohdeOid2);
            TavallinenValinnanVaiheTyyppi valinnanvaihe3 = luoTavallinenValinnanvaihe(valinnanvaiheOid3, 0);
            valinnanvaihe3.getValintatapajono().add(luoValintatapajono(valintatapajonoOid4, 1, 40, luoJarjestyskriteeri(viisisataa, 1), luoJarjestyskriteeri(kuusisataa, 2)));
            valinnanvaihe3.getValintatapajono().add(luoValintatapajono(valintatapajonoOid5, 2, 50, luoJarjestyskriteeri(seitsemansataa, 1)));
            valintaperusteet3.setValinnanVaihe(valinnanvaihe3);

            ValintaperusteetTyyppi valintaperusteet4 = luoValintaperusteet(hakuOid, hakukohdeOid2);
            TavallinenValinnanVaiheTyyppi valinnanvaihe4 = luoTavallinenValinnanvaihe(valinnanvaiheOid4, 1);
            valinnanvaihe4.getValintatapajono().add(luoValintatapajono(valintatapajonoOid6, 1, 60, luoJarjestyskriteeri(kahdeksansataa, 1)));
            valintaperusteet4.setValinnanVaihe(valinnanvaihe4);

            HakemusTyyppi hakemus1 = luoHakemus(hakemusOid1, hakijaOid1, hakukohdeOid1, hakukohdeOid2);
            HakemusTyyppi hakemus2 = luoHakemus(hakemusOid2, hakijaOid2, hakukohdeOid2, hakukohdeOid1);

            valintalaskentaSuorittajaService.suoritaLaskenta(Arrays.asList(hakemus1, hakemus2), Arrays.asList(valintaperusteet2, valintaperusteet1, valintaperusteet4, valintaperusteet3));
        }

        {
            Valinnanvaihe valinnanvaihe1 = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid1);
            assertEquals(hakuOid, valinnanvaihe1.getHakuOid());
            assertEquals(hakukohdeOid1, valinnanvaihe1.getHakukohdeOid());
            assertEquals(0, valinnanvaihe1.getJarjestysnumero());
            assertEquals(valinnanvaiheOid1, valinnanvaihe1.getValinnanvaiheOid());
            assertEquals(2, valinnanvaihe1.getValintatapajonot().size());

            Comparator<Jonosija> jonosijaComparator = new Comparator<Jonosija>() {
                @Override
                public int compare(Jonosija o1, Jonosija o2) {
                    return o1.getHakemusOid().compareTo(o2.getHakemusOid());
                }
            };

            {
                Valintatapajono jono = valinnanvaihe1.getValintatapajonot().get(0);
                assertEquals(10, jono.getAloituspaikat());
                assertEquals(1, jono.getPrioriteetti());
                assertEquals(valintatapajonoOid1, jono.getValintatapajonoOid());


                assertEquals(2, jono.getJonosijat().size());
                Collections.sort(jono.getJonosijat(), jonosijaComparator);

                {
                    Jonosija jonosija1 = jono.getJonosijat().get(0);
                    assertEquals(hakemusOid1, jonosija1.getHakemusOid());
                    assertEquals(hakijaOid1, jonosija1.getHakijaOid());
                    assertEquals(1, jonosija1.getHakutoiveprioriteetti());
                    assertEquals(2, jonosija1.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos1 = jonosija1.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("100.0"), jarjestyskriteeritulos1.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos1.getTila());
                    assertEquals(1, jarjestyskriteeritulos1.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos1.getHistoria());
                    Jarjestyskriteerihistoria historia1 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
                    assertNotNull(historia1);
                    assertNotNull(historia1.getHistoria());

                    Jarjestyskriteeritulos jarjestyskriteeritulos2 = jonosija1.getJarjestyskriteeritulokset().get(1);
                    assertEquals(new BigDecimal("200.0"), jarjestyskriteeritulos2.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
                    assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos2.getHistoria());
                    Jarjestyskriteerihistoria historia2 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
                    assertNotNull(historia2);
                    assertNotNull(historia2.getHistoria());
                }

                {
                    Jonosija jonosija2 = jono.getJonosijat().get(1);
                    assertEquals(hakemusOid2, jonosija2.getHakemusOid());
                    assertEquals(hakijaOid2, jonosija2.getHakijaOid());
                    assertEquals(2, jonosija2.getHakutoiveprioriteetti());
                    assertEquals(2, jonosija2.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos1 = jonosija2.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("100.0"), jarjestyskriteeritulos1.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos1.getTila());
                    assertEquals(1, jarjestyskriteeritulos1.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos1.getHistoria());
                    Jarjestyskriteerihistoria historia1 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
                    assertNotNull(historia1);
                    assertNotNull(historia1.getHistoria());

                    Jarjestyskriteeritulos jarjestyskriteeritulos2 = jonosija2.getJarjestyskriteeritulokset().get(1);
                    assertEquals(new BigDecimal("200.0"), jarjestyskriteeritulos2.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
                    assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos2.getHistoria());
                    Jarjestyskriteerihistoria historia2 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
                    assertNotNull(historia2);
                    assertNotNull(historia2.getHistoria());
                }
            }
            {
                Valintatapajono jono = valinnanvaihe1.getValintatapajonot().get(1);
                assertEquals(20, jono.getAloituspaikat());
                assertEquals(2, jono.getPrioriteetti());
                assertEquals(valintatapajonoOid2, jono.getValintatapajonoOid());

                assertEquals(2, jono.getJonosijat().size());
                Collections.sort(jono.getJonosijat(), jonosijaComparator);

                {
                    Jonosija jonosija = jono.getJonosijat().get(0);
                    assertEquals(hakemusOid1, jonosija.getHakemusOid());
                    assertEquals(hakijaOid1, jonosija.getHakijaOid());
                    assertEquals(1, jonosija.getHakutoiveprioriteetti());
                    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("300.0"), jarjestyskriteeritulos.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
                    assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos.getHistoria());
                    Jarjestyskriteerihistoria historia = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
                    assertNotNull(historia);
                    assertNotNull(historia.getHistoria());
                }

                {
                    Jonosija jonosija = jono.getJonosijat().get(1);
                    assertEquals(hakemusOid2, jonosija.getHakemusOid());
                    assertEquals(hakijaOid2, jonosija.getHakijaOid());
                    assertEquals(2, jonosija.getHakutoiveprioriteetti());
                    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("300.0"), jarjestyskriteeritulos.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
                    assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos.getHistoria());
                    Jarjestyskriteerihistoria historia = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
                    assertNotNull(historia);
                    assertNotNull(historia.getHistoria());
                }
            }
        }


        {
            Valinnanvaihe valinnanvaihe2 = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid2);
            assertEquals(hakuOid, valinnanvaihe2.getHakuOid());
            assertEquals(hakukohdeOid1, valinnanvaihe2.getHakukohdeOid());
            assertEquals(1, valinnanvaihe2.getJarjestysnumero());
            assertEquals(valinnanvaiheOid2, valinnanvaihe2.getValinnanvaiheOid());
            assertEquals(1, valinnanvaihe2.getValintatapajonot().size());

            Comparator<Jonosija> jonosijaComparator = new Comparator<Jonosija>() {
                @Override
                public int compare(Jonosija o1, Jonosija o2) {
                    return o1.getHakemusOid().compareTo(o2.getHakemusOid());
                }
            };

            {
                Valintatapajono jono = valinnanvaihe2.getValintatapajonot().get(0);
                assertEquals(30, jono.getAloituspaikat());
                assertEquals(1, jono.getPrioriteetti());
                assertEquals(valintatapajonoOid3, jono.getValintatapajonoOid());


                assertEquals(2, jono.getJonosijat().size());
                Collections.sort(jono.getJonosijat(), jonosijaComparator);

                {
                    Jonosija jonosija = jono.getJonosijat().get(0);
                    assertEquals(hakemusOid1, jonosija.getHakemusOid());
                    assertEquals(hakijaOid1, jonosija.getHakijaOid());
                    assertEquals(1, jonosija.getHakutoiveprioriteetti());
                    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("400.0"), jarjestyskriteeritulos.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
                    assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos.getHistoria());
                    Jarjestyskriteerihistoria historia1 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
                    assertNotNull(historia1);
                    assertNotNull(historia1.getHistoria());
                }
                {
                    Jonosija jonosija = jono.getJonosijat().get(1);
                    assertEquals(hakemusOid2, jonosija.getHakemusOid());
                    assertEquals(hakijaOid2, jonosija.getHakijaOid());
                    assertEquals(2, jonosija.getHakutoiveprioriteetti());
                    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("400.0"), jarjestyskriteeritulos.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
                    assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos.getHistoria());
                    Jarjestyskriteerihistoria historia1 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
                    assertNotNull(historia1);
                    assertNotNull(historia1.getHistoria());
                }
            }
        }


        ////////////
        {
            Valinnanvaihe valinnanvaihe3 = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid3);
            assertEquals(hakuOid, valinnanvaihe3.getHakuOid());
            assertEquals(hakukohdeOid2, valinnanvaihe3.getHakukohdeOid());
            assertEquals(0, valinnanvaihe3.getJarjestysnumero());
            assertEquals(valinnanvaiheOid3, valinnanvaihe3.getValinnanvaiheOid());
            assertEquals(2, valinnanvaihe3.getValintatapajonot().size());

            Comparator<Jonosija> jonosijaComparator = new Comparator<Jonosija>() {
                @Override
                public int compare(Jonosija o1, Jonosija o2) {
                    return o1.getHakemusOid().compareTo(o2.getHakemusOid());
                }
            };

            {
                Valintatapajono jono = valinnanvaihe3.getValintatapajonot().get(0);
                assertEquals(40, jono.getAloituspaikat());
                assertEquals(1, jono.getPrioriteetti());
                assertEquals(valintatapajonoOid4, jono.getValintatapajonoOid());

                assertEquals(2, jono.getJonosijat().size());
                Collections.sort(jono.getJonosijat(), jonosijaComparator);

                {
                    Jonosija jonosija1 = jono.getJonosijat().get(0);
                    assertEquals(hakemusOid1, jonosija1.getHakemusOid());
                    assertEquals(hakijaOid1, jonosija1.getHakijaOid());
                    assertEquals(2, jonosija1.getHakutoiveprioriteetti());
                    assertEquals(2, jonosija1.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos1 = jonosija1.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("500.0"), jarjestyskriteeritulos1.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos1.getTila());
                    assertEquals(1, jarjestyskriteeritulos1.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos1.getHistoria());
                    Jarjestyskriteerihistoria historia1 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
                    assertNotNull(historia1);
                    assertNotNull(historia1.getHistoria());

                    Jarjestyskriteeritulos jarjestyskriteeritulos2 = jonosija1.getJarjestyskriteeritulokset().get(1);
                    assertEquals(new BigDecimal("600.0"), jarjestyskriteeritulos2.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
                    assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos2.getHistoria());
                    Jarjestyskriteerihistoria historia2 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
                    assertNotNull(historia2);
                    assertNotNull(historia2.getHistoria());
                }

                {
                    Jonosija jonosija2 = jono.getJonosijat().get(1);
                    assertEquals(hakemusOid2, jonosija2.getHakemusOid());
                    assertEquals(hakijaOid2, jonosija2.getHakijaOid());
                    assertEquals(1, jonosija2.getHakutoiveprioriteetti());
                    assertEquals(2, jonosija2.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos1 = jonosija2.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("500.0"), jarjestyskriteeritulos1.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos1.getTila());
                    assertEquals(1, jarjestyskriteeritulos1.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos1.getHistoria());
                    Jarjestyskriteerihistoria historia1 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
                    assertNotNull(historia1);
                    assertNotNull(historia1.getHistoria());

                    Jarjestyskriteeritulos jarjestyskriteeritulos2 = jonosija2.getJarjestyskriteeritulokset().get(1);
                    assertEquals(new BigDecimal("600.0"), jarjestyskriteeritulos2.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
                    assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos2.getHistoria());
                    Jarjestyskriteerihistoria historia2 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
                    assertNotNull(historia2);
                    assertNotNull(historia2.getHistoria());
                }
            }
            {
                Valintatapajono jono = valinnanvaihe3.getValintatapajonot().get(1);
                assertEquals(50, jono.getAloituspaikat());
                assertEquals(2, jono.getPrioriteetti());
                assertEquals(valintatapajonoOid5, jono.getValintatapajonoOid());

                assertEquals(2, jono.getJonosijat().size());
                Collections.sort(jono.getJonosijat(), jonosijaComparator);

                {
                    Jonosija jonosija = jono.getJonosijat().get(0);
                    assertEquals(hakemusOid1, jonosija.getHakemusOid());
                    assertEquals(hakijaOid1, jonosija.getHakijaOid());
                    assertEquals(2, jonosija.getHakutoiveprioriteetti());
                    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("700.0"), jarjestyskriteeritulos.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
                    assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos.getHistoria());
                    Jarjestyskriteerihistoria historia = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
                    assertNotNull(historia);
                    assertNotNull(historia.getHistoria());
                }

                {
                    Jonosija jonosija = jono.getJonosijat().get(1);
                    assertEquals(hakemusOid2, jonosija.getHakemusOid());
                    assertEquals(hakijaOid2, jonosija.getHakijaOid());
                    assertEquals(1, jonosija.getHakutoiveprioriteetti());
                    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("700.0"), jarjestyskriteeritulos.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
                    assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos.getHistoria());
                    Jarjestyskriteerihistoria historia = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
                    assertNotNull(historia);
                    assertNotNull(historia.getHistoria());
                }
            }
        }
        {
            Valinnanvaihe valinnanvaihe4 = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid4);
            assertEquals(hakuOid, valinnanvaihe4.getHakuOid());
            assertEquals(hakukohdeOid2, valinnanvaihe4.getHakukohdeOid());
            assertEquals(1, valinnanvaihe4.getJarjestysnumero());
            assertEquals(valinnanvaiheOid4, valinnanvaihe4.getValinnanvaiheOid());
            assertEquals(1, valinnanvaihe4.getValintatapajonot().size());

            Comparator<Jonosija> jonosijaComparator = new Comparator<Jonosija>() {
                @Override
                public int compare(Jonosija o1, Jonosija o2) {
                    return o1.getHakemusOid().compareTo(o2.getHakemusOid());
                }
            };

            {
                Valintatapajono jono = valinnanvaihe4.getValintatapajonot().get(0);
                assertEquals(60, jono.getAloituspaikat());
                assertEquals(1, jono.getPrioriteetti());
                assertEquals(valintatapajonoOid6, jono.getValintatapajonoOid());

                assertEquals(2, jono.getJonosijat().size());
                Collections.sort(jono.getJonosijat(), jonosijaComparator);

                {
                    Jonosija jonosija = jono.getJonosijat().get(0);
                    assertEquals(hakemusOid1, jonosija.getHakemusOid());
                    assertEquals(hakijaOid1, jonosija.getHakijaOid());
                    assertEquals(2, jonosija.getHakutoiveprioriteetti());
                    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("800.0"), jarjestyskriteeritulos.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
                    assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos.getHistoria());
                    Jarjestyskriteerihistoria historia1 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
                    assertNotNull(historia1);
                    assertNotNull(historia1.getHistoria());
                }
                {
                    Jonosija jonosija = jono.getJonosijat().get(1);
                    assertEquals(hakemusOid2, jonosija.getHakemusOid());
                    assertEquals(hakijaOid2, jonosija.getHakijaOid());
                    assertEquals(1, jonosija.getHakutoiveprioriteetti());
                    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

                    Jarjestyskriteeritulos jarjestyskriteeritulos = jonosija.getJarjestyskriteeritulokset().get(0);
                    assertEquals(new BigDecimal("800.0"), jarjestyskriteeritulos.getArvo());
                    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
                    assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
                    assertNotNull(jarjestyskriteeritulos.getHistoria());
                    Jarjestyskriteerihistoria historia1 = jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
                    assertNotNull(historia1);
                    assertNotNull(historia1.getHistoria());
                }
            }
        }
    }
}
