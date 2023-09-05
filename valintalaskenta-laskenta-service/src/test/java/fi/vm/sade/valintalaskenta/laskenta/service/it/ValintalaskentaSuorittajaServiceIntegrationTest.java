package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static com.github.npathai.hamcrestopt.OptionalMatchers.isPresent;
import static fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi.LUKUARVO;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoJarjestyskriteeri;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoTavallinenValinnanvaihe;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoValintaperusteet;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoValintaperusteetJaTavallinenValinnanvaihe;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoValintatapajono;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import fi.vm.sade.service.valintaperusteet.dto.SyoteparametriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetFunktiokutsuDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(
    listeners = {
      DependencyInjectionTestExecutionListener.class,
      DirtiesContextTestExecutionListener.class
    })
public class ValintalaskentaSuorittajaServiceIntegrationTest {
  private final String uuid = null;

  @Autowired private ApplicationContext applicationContext;

  @Autowired private ValintalaskentaSuorittajaService valintalaskentaSuorittajaService;

  @Autowired private ValinnanvaiheDAO valinnanvaiheDAO;

  @Autowired private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

  private static ValintaperusteetFunktiokutsuDTO sata;
  private static ValintaperusteetFunktiokutsuDTO kaksisataa;
  private static ValintaperusteetFunktiokutsuDTO kolmesataa;
  private static ValintaperusteetFunktiokutsuDTO neljasataa;
  private static ValintaperusteetFunktiokutsuDTO viisisataa;
  private static ValintaperusteetFunktiokutsuDTO kuusisataa;
  private static ValintaperusteetFunktiokutsuDTO seitsemansataa;
  private static ValintaperusteetFunktiokutsuDTO kahdeksansataa;

  private final boolean korkeakouluhaku = false;

  static {
    sata = new ValintaperusteetFunktiokutsuDTO();
    sata.setFunktionimi(LUKUARVO);
    sata.setTallennaTulos(true);
    sata.setTulosTunniste("sata");
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("luku");
      param.setArvo("100.0");
      sata.getSyoteparametrit().add(param);
    }

    kaksisataa = new ValintaperusteetFunktiokutsuDTO();
    kaksisataa.setFunktionimi(LUKUARVO);
    kaksisataa.setTallennaTulos(true);
    kaksisataa.setTulosTunniste("kaksisataa");
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("luku");
      param.setArvo("200.0");
      kaksisataa.getSyoteparametrit().add(param);
    }

    kolmesataa = new ValintaperusteetFunktiokutsuDTO();
    kolmesataa.setFunktionimi(LUKUARVO);
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("luku");
      param.setArvo("300.0");
      kolmesataa.getSyoteparametrit().add(param);
    }

    neljasataa = new ValintaperusteetFunktiokutsuDTO();
    neljasataa.setFunktionimi(LUKUARVO);
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("luku");
      param.setArvo("400.0");
      neljasataa.getSyoteparametrit().add(param);
    }

    viisisataa = new ValintaperusteetFunktiokutsuDTO();
    viisisataa.setFunktionimi(LUKUARVO);
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("luku");
      param.setArvo("500.0");
      viisisataa.getSyoteparametrit().add(param);
    }

    kuusisataa = new ValintaperusteetFunktiokutsuDTO();
    kuusisataa.setFunktionimi(LUKUARVO);
    kuusisataa.setTallennaTulos(true);
    kuusisataa.setTulosTunniste("kuusisataa");
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("luku");
      param.setArvo("600.0");
      kuusisataa.getSyoteparametrit().add(param);
    }

    seitsemansataa = new ValintaperusteetFunktiokutsuDTO();
    seitsemansataa.setFunktionimi(LUKUARVO);
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("luku");
      param.setArvo("700.0");
      seitsemansataa.getSyoteparametrit().add(param);
    }

    kahdeksansataa = new ValintaperusteetFunktiokutsuDTO();
    kahdeksansataa.setFunktionimi(LUKUARVO);
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
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
      ValintaperusteetDTO valintaperusteet1 = luoValintaperusteet(hakuOid, hakukohdeOid1);
      ValintaperusteetValinnanVaiheDTO valinnanvaihe1 =
          luoTavallinenValinnanvaihe(valinnanvaiheOid1, 0);
      valinnanvaihe1
          .getValintatapajono()
          .add(
              luoValintatapajono(
                  valintatapajonoOid1,
                  1,
                  10,
                  luoJarjestyskriteeri(sata, 1),
                  luoJarjestyskriteeri(kaksisataa, 2)));
      valinnanvaihe1
          .getValintatapajono()
          .add(luoValintatapajono(valintatapajonoOid2, 2, 20, luoJarjestyskriteeri(kolmesataa, 1)));
      valintaperusteet1.setValinnanVaihe(valinnanvaihe1);

      ValintaperusteetDTO valintaperusteet2 = luoValintaperusteet(hakuOid, hakukohdeOid1);
      ValintaperusteetValinnanVaiheDTO valinnanvaihe2 =
          luoTavallinenValinnanvaihe(valinnanvaiheOid2, 1);
      valinnanvaihe2
          .getValintatapajono()
          .add(luoValintatapajono(valintatapajonoOid3, 1, 30, luoJarjestyskriteeri(neljasataa, 1)));
      valintaperusteet2.setValinnanVaihe(valinnanvaihe2);

      ValintaperusteetDTO valintaperusteet3 = luoValintaperusteet(hakuOid, hakukohdeOid2);
      ValintaperusteetValinnanVaiheDTO valinnanvaihe3 =
          luoTavallinenValinnanvaihe(valinnanvaiheOid3, 0);
      valinnanvaihe3
          .getValintatapajono()
          .add(
              luoValintatapajono(
                  valintatapajonoOid4,
                  1,
                  40,
                  luoJarjestyskriteeri(viisisataa, 1),
                  luoJarjestyskriteeri(kuusisataa, 2)));
      valinnanvaihe3
          .getValintatapajono()
          .add(
              luoValintatapajono(
                  valintatapajonoOid5, 2, 50, luoJarjestyskriteeri(seitsemansataa, 1)));
      valintaperusteet3.setValinnanVaihe(valinnanvaihe3);

      ValintaperusteetDTO valintaperusteet4 = luoValintaperusteet(hakuOid, hakukohdeOid2);
      ValintaperusteetValinnanVaiheDTO valinnanvaihe4 =
          luoTavallinenValinnanvaihe(valinnanvaiheOid4, 1);
      valinnanvaihe4
          .getValintatapajono()
          .add(
              luoValintatapajono(
                  valintatapajonoOid6, 1, 60, luoJarjestyskriteeri(kahdeksansataa, 1)));
      valintaperusteet4.setValinnanVaihe(valinnanvaihe4);

      HakemusDTO hakemus1 =
          luoHakemus(hakuOid, hakemusOid1, hakijaOid1, hakukohdeOid1, hakukohdeOid2);
      HakemusDTO hakemus2 =
          luoHakemus(hakuOid, hakemusOid2, hakijaOid2, hakukohdeOid2, hakukohdeOid1);

      valintalaskentaSuorittajaService.suoritaLaskenta(
          Arrays.asList(hakemus1, hakemus2),
          Arrays.asList(valintaperusteet2, valintaperusteet1),
          new ArrayList<>(),
          hakukohdeOid1,
          uuid,
          korkeakouluhaku);
      valintalaskentaSuorittajaService.suoritaLaskenta(
          Arrays.asList(hakemus1, hakemus2),
          Arrays.asList(valintaperusteet4, valintaperusteet3),
          new ArrayList<>(),
          hakukohdeOid2,
          uuid,
          korkeakouluhaku);
    }

    {
      Valinnanvaihe valinnanvaihe1 = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid1);
      assertEquals(hakuOid, valinnanvaihe1.getHakuOid());
      assertEquals(hakukohdeOid1, valinnanvaihe1.getHakukohdeOid());
      assertEquals(0, valinnanvaihe1.getJarjestysnumero());
      assertEquals(valinnanvaiheOid1, valinnanvaihe1.getValinnanvaiheOid());
      assertEquals(2, valinnanvaihe1.getValintatapajonot().size());

      Comparator<Jonosija> jonosijaComparator = Comparator.comparing(Jonosija::getHakemusOid);

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

          Jarjestyskriteeritulos jarjestyskriteeritulos1 =
              jonosija1.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("100.0"), jarjestyskriteeritulos1.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos1.getTila());
          assertEquals(1, jarjestyskriteeritulos1.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos1.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());

          Jarjestyskriteeritulos jarjestyskriteeritulos2 =
              jonosija1.getJarjestyskriteeritulokset().get(1);
          assertEquals(new BigDecimal("200.0"), jarjestyskriteeritulos2.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
          assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos2.getHistoria());
          Jarjestyskriteerihistoria historia2 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
          assertNotNull(historia2);
          assertNotNull(historia2.getHistoria());
          assertNotNull(historia2.getHistoriaGzip());

          assertEquals(2, jonosija1.getFunktioTulokset().size());
          assertEquals("100.0", jonosija1.getFunktioTulokset().get(0).getArvo());
          assertEquals("200.0", jonosija1.getFunktioTulokset().get(1).getArvo());
        }

        {
          Jonosija jonosija2 = jono.getJonosijat().get(1);
          assertEquals(hakemusOid2, jonosija2.getHakemusOid());
          assertEquals(hakijaOid2, jonosija2.getHakijaOid());
          assertEquals(2, jonosija2.getHakutoiveprioriteetti());
          assertEquals(2, jonosija2.getJarjestyskriteeritulokset().size());

          Jarjestyskriteeritulos jarjestyskriteeritulos1 =
              jonosija2.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("100.0"), jarjestyskriteeritulos1.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos1.getTila());
          assertEquals(1, jarjestyskriteeritulos1.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos1.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());
          assertNotNull(historia1.getHistoriaGzip());

          Jarjestyskriteeritulos jarjestyskriteeritulos2 =
              jonosija2.getJarjestyskriteeritulokset().get(1);
          assertEquals(new BigDecimal("200.0"), jarjestyskriteeritulos2.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
          assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos2.getHistoria());
          Jarjestyskriteerihistoria historia2 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
          assertNotNull(historia2);
          assertNotNull(historia2.getHistoria());
          assertNotNull(historia2.getHistoriaGzip());
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

          Jarjestyskriteeritulos jarjestyskriteeritulos =
              jonosija.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("300.0"), jarjestyskriteeritulos.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
          assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia);
          assertNotNull(historia.getHistoria());
        }

        {
          Jonosija jonosija = jono.getJonosijat().get(1);
          assertEquals(hakemusOid2, jonosija.getHakemusOid());
          assertEquals(hakijaOid2, jonosija.getHakijaOid());
          assertEquals(2, jonosija.getHakutoiveprioriteetti());
          assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

          Jarjestyskriteeritulos jarjestyskriteeritulos =
              jonosija.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("300.0"), jarjestyskriteeritulos.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
          assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
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

      Comparator<Jonosija> jonosijaComparator = Comparator.comparing(Jonosija::getHakemusOid);

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

          Jarjestyskriteeritulos jarjestyskriteeritulos =
              jonosija.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("400.0"), jarjestyskriteeritulos.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
          assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());

          assertEquals(0, jonosija.getFunktioTulokset().size());
        }
        {
          Jonosija jonosija = jono.getJonosijat().get(1);
          assertEquals(hakemusOid2, jonosija.getHakemusOid());
          assertEquals(hakijaOid2, jonosija.getHakijaOid());
          assertEquals(2, jonosija.getHakutoiveprioriteetti());
          assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

          Jarjestyskriteeritulos jarjestyskriteeritulos =
              jonosija.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("400.0"), jarjestyskriteeritulos.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
          assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
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

      Comparator<Jonosija> jonosijaComparator = Comparator.comparing(Jonosija::getHakemusOid);

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

          Jarjestyskriteeritulos jarjestyskriteeritulos1 =
              jonosija1.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("500.0"), jarjestyskriteeritulos1.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos1.getTila());
          assertEquals(1, jarjestyskriteeritulos1.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos1.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());

          Jarjestyskriteeritulos jarjestyskriteeritulos2 =
              jonosija1.getJarjestyskriteeritulokset().get(1);
          assertEquals(new BigDecimal("600.0"), jarjestyskriteeritulos2.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
          assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos2.getHistoria());
          Jarjestyskriteerihistoria historia2 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
          assertNotNull(historia2);
          assertNotNull(historia2.getHistoria());
          assertEquals(1, jonosija1.getFunktioTulokset().size());
          assertEquals("600.0", jonosija1.getFunktioTulokset().get(0).getArvo());
        }

        {
          Jonosija jonosija2 = jono.getJonosijat().get(1);
          assertEquals(hakemusOid2, jonosija2.getHakemusOid());
          assertEquals(hakijaOid2, jonosija2.getHakijaOid());
          assertEquals(1, jonosija2.getHakutoiveprioriteetti());
          assertEquals(2, jonosija2.getJarjestyskriteeritulokset().size());

          Jarjestyskriteeritulos jarjestyskriteeritulos1 =
              jonosija2.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("500.0"), jarjestyskriteeritulos1.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos1.getTila());
          assertEquals(1, jarjestyskriteeritulos1.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos1.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());

          Jarjestyskriteeritulos jarjestyskriteeritulos2 =
              jonosija2.getJarjestyskriteeritulokset().get(1);
          assertEquals(new BigDecimal("600.0"), jarjestyskriteeritulos2.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
          assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos2.getHistoria());
          Jarjestyskriteerihistoria historia2 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
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

          Jarjestyskriteeritulos jarjestyskriteeritulos =
              jonosija.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("700.0"), jarjestyskriteeritulos.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
          assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia);
          assertNotNull(historia.getHistoria());
        }

        {
          Jonosija jonosija = jono.getJonosijat().get(1);
          assertEquals(hakemusOid2, jonosija.getHakemusOid());
          assertEquals(hakijaOid2, jonosija.getHakijaOid());
          assertEquals(1, jonosija.getHakutoiveprioriteetti());
          assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

          Jarjestyskriteeritulos jarjestyskriteeritulos =
              jonosija.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("700.0"), jarjestyskriteeritulos.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
          assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
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

      Comparator<Jonosija> jonosijaComparator = Comparator.comparing(Jonosija::getHakemusOid);

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

          Jarjestyskriteeritulos jarjestyskriteeritulos =
              jonosija.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("800.0"), jarjestyskriteeritulos.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
          assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());
        }
        {
          Jonosija jonosija = jono.getJonosijat().get(1);
          assertEquals(hakemusOid2, jonosija.getHakemusOid());
          assertEquals(hakijaOid2, jonosija.getHakijaOid());
          assertEquals(1, jonosija.getHakutoiveprioriteetti());
          assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

          Jarjestyskriteeritulos jarjestyskriteeritulos =
              jonosija.getJarjestyskriteeritulokset().get(0);
          assertEquals(new BigDecimal("800.0"), jarjestyskriteeritulos.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos.getTila());
          assertEquals(1, jarjestyskriteeritulos.getPrioriteetti());
          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());
        }
      }
    }
  }

  @Test
  public void testViimeisinValinnanVaihe() {
    final String hakemusOid = "1.2.246.562.11.00000072753";
    final String hakukohdeOid = "1.2.246.562.5.91937845484";
    final String hakuOid = "1.2.246.562.5.2013080813081926341927";
    final String valinnanVaiheOid = "vv3";
    final String valintatapajonoOid = "jono1";

    ValintaperusteetDTO vv3 =
        luoValintaperusteetJaTavallinenValinnanvaihe(hakuOid, hakukohdeOid, valinnanVaiheOid, 2);
    (vv3.getValinnanVaihe())
        .getValintatapajono()
        .add(luoValintatapajono(valintatapajonoOid, 0, 10, luoJarjestyskriteeri(sata, 1)));
    valintalaskentaSuorittajaService.suoritaLaskenta(
        Collections.singletonList(luoHakemus(hakuOid, hakemusOid, hakemusOid, hakukohdeOid)),
        Collections.singletonList(vv3),
        new ArrayList<>(),
        hakukohdeOid,
        uuid,
        korkeakouluhaku);

    Valinnanvaihe vaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanVaiheOid);
    assertNotNull(vaihe);
    assertEquals(valinnanVaiheOid, vaihe.getValinnanvaiheOid());
    assertEquals(1, vaihe.getValintatapajonot().size());

    Valintatapajono jono = vaihe.getValintatapajonot().get(0);
    assertEquals(valintatapajonoOid, jono.getValintatapajonoOid());
    assertEquals(1, jono.getJonosijat().size());

    Jonosija jonosija = jono.getJonosijat().get(0);
    assertEquals(hakemusOid, jonosija.getHakemusOid());
    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

    Jarjestyskriteeritulos tulos = jonosija.getJarjestyskriteeritulokset().get(0);
    assertEquals(JarjestyskriteerituloksenTila.HYLATTY, tulos.getTila());
  }

  @Test
  public void testPoistaHylatyt() {
    final String hakemusOid = "1.2.246.562.11.00000072753"; // Hylätty edellisessä vaiheessa
    final String hakemusOid2 = "1.2.246.562.11.00000072672";
    final String hakukohdeOid = "1.2.246.562.5.91937845484";
    final String hakuOid = "1.2.246.562.5.2013080813081926341927";
    final String valinnanVaiheOid = "vv3";
    final String valintatapajonoOid = "jono1";

    ValintaperusteetDTO vv3 =
        luoValintaperusteetJaTavallinenValinnanvaihe(hakuOid, hakukohdeOid, valinnanVaiheOid, 2);
    (vv3.getValinnanVaihe())
        .getValintatapajono()
        .add(luoValintatapajono(valintatapajonoOid, 0, 10, luoJarjestyskriteeri(sata, 1)));
    (vv3.getValinnanVaihe()).getValintatapajono().get(0).setPoistetaankoHylatyt(true);
    valintalaskentaSuorittajaService.suoritaLaskenta(
        Arrays.asList(
            luoHakemus(hakuOid, hakemusOid, hakemusOid, hakukohdeOid),
            luoHakemus(hakuOid, hakemusOid2, hakemusOid, hakukohdeOid)),
        Collections.singletonList(vv3),
        new ArrayList<>(),
        hakukohdeOid,
        uuid,
        korkeakouluhaku);

    Valinnanvaihe vaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanVaiheOid);
    assertNotNull(vaihe);
    assertEquals(valinnanVaiheOid, vaihe.getValinnanvaiheOid());
    assertEquals(1, vaihe.getValintatapajonot().size());

    Valintatapajono jono = vaihe.getValintatapajonot().get(0);
    assertEquals(valintatapajonoOid, jono.getValintatapajonoOid());
    assertEquals(1, jono.getJonosijat().size());

    Jonosija jonosija = jono.getJonosijat().get(0);
    assertEquals(hakemusOid2, jonosija.getHakemusOid());
    assertEquals(1, jonosija.getJarjestyskriteeritulokset().size());

    Jarjestyskriteeritulos tulos = jonosija.getJarjestyskriteeritulokset().get(0);
    assertEquals(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, tulos.getTila());
  }

  @Test
  public void testValisijoitteluHylkaysHyvaksytty() {

    final String valinnanVaiheOid = "vv3";
    final String valintatapajonoOid = "jono1";
    final String hakemusOid = "1.2.246.562.11.00001212279";
    final String hakukohdeOid = "1.2.246.562.20.66128426039";
    final String hakuOid = "1.2.246.562.29.173465377510";
    final String hakemusOid2 = "1.2.246.562.11.00001223556";

    ValintaperusteetDTO vv3 =
        luoValintaperusteetJaTavallinenValinnanvaihe(hakuOid, hakukohdeOid, valinnanVaiheOid, 3);
    (vv3.getValinnanVaihe())
        .getValintatapajono()
        .add(luoValintatapajono(valintatapajonoOid, 0, 10, luoJarjestyskriteeri(sata, 1)));
    valintalaskentaSuorittajaService.suoritaLaskenta(
        Arrays.asList(
            luoHakemus(hakuOid, hakemusOid, hakemusOid, hakukohdeOid),
            luoHakemus(hakuOid, hakemusOid2, hakemusOid, hakukohdeOid)),
        Collections.singletonList(vv3),
        new ArrayList<>(),
        hakukohdeOid,
        uuid,
        korkeakouluhaku);

    Valinnanvaihe vaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanVaiheOid);
    assertNotNull(vaihe);

    assertEquals(
        JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA,
        vaihe.getValintatapajonot().get(0).getJonosijat().stream()
            .filter(j -> j.getHakemusOid().equals(hakemusOid))
            .findFirst()
            .get()
            .getJarjestyskriteeritulokset()
            .get(0)
            .getTila());
    assertEquals(
        JarjestyskriteerituloksenTila.HYLATTY,
        vaihe.getValintatapajonot().get(0).getJonosijat().stream()
            .filter(j -> j.getHakemusOid().equals(hakemusOid2))
            .findFirst()
            .get()
            .getJarjestyskriteeritulokset()
            .get(0)
            .getTila());
    assertEquals(
        "Pisteesi eivät riittäneet valintakoekutsuun",
        vaihe.getValintatapajonot().get(0).getJonosijat().stream()
            .filter(j -> j.getHakemusOid().equals(hakemusOid2))
            .findFirst()
            .get()
            .getJarjestyskriteeritulokset()
            .get(0)
            .getKuvaus()
            .get("FI"));
  }

  @Test
  public void
      valisijoittelussaVoiTullaHyvaksytyksiVaikkaToiseltaKohteeltaEiLoytyisiKutsujaKaikkiinKohteenKokeisiin() {
    final String valinnanVaiheOid = "vv4";
    final String valintatapajonoOid = "jono2";
    final String hakemusOid = "1.2.246.562.11.00001212279";
    final String hakukohdeOid = "1.2.246.562.20.66128426039";
    final String hakuOid = "1.2.246.562.29.173465377510";

    ValintaperusteetDTO vv3 =
        luoValintaperusteetJaTavallinenValinnanvaihe(hakuOid, hakukohdeOid, valinnanVaiheOid, 3);
    (vv3.getValinnanVaihe())
        .getValintatapajono()
        .add(luoValintatapajono(valintatapajonoOid, 0, 10, luoJarjestyskriteeri(sata, 1)));
    valintalaskentaSuorittajaService.suoritaLaskenta(
        Collections.singletonList(luoHakemus(hakuOid, hakemusOid, hakemusOid, hakukohdeOid)),
        Collections.singletonList(vv3),
        new ArrayList<>(),
        hakukohdeOid,
        uuid,
        korkeakouluhaku);

    Valinnanvaihe vaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanVaiheOid);
    assertNotNull(vaihe);

    Optional<Valintatapajono> jononTulos =
        vaihe.getValintatapajonot().stream()
            .filter(j -> valintatapajonoOid.equals(j.getValintatapajonoOid()))
            .findFirst();
    assertThat(jononTulos, isPresent());

    Optional<Jonosija> hakemuksenTulos =
        jononTulos.get().getJonosijat().stream()
            .filter(s -> hakemusOid.equals(s.getHakemusOid()))
            .findFirst();
    assertThat(hakemuksenTulos, isPresent());

    List<Jarjestyskriteeritulos> jarjestyskriteeritulokset =
        hakemuksenTulos.get().getJarjestyskriteeritulokset();
    assertThat(jarjestyskriteeritulokset, hasSize(1));
    assertEquals(HYVAKSYTTAVISSA, jarjestyskriteeritulokset.get(0).getTila());
  }
}
