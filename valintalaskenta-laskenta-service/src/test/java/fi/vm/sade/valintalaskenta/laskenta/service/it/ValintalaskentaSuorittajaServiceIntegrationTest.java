package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static com.github.npathai.hamcrestopt.OptionalMatchers.isPresent;
import static fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi.LUKUARVO;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYLATTY;
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
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;
import fi.vm.sade.valintalaskenta.testing.AbstractMocklessIntegrationTest;
import java.math.BigDecimal;
import java.util.*;

import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

public class ValintalaskentaSuorittajaServiceIntegrationTest
    extends AbstractMocklessIntegrationTest {
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
      assertEquals(valinnanvaiheOid1, valinnanvaihe1.getValinnanVaiheOid());
      assertEquals(2, valinnanvaihe1.getValintatapajono().size());

      Comparator<Jonosija> jonosijaComparator = Comparator.comparing(Jonosija::getHakemusOid);

      {
        Valintatapajono jono = valinnanvaihe1.getValintatapajono().get(0);
        assertEquals(10, jono.getAloituspaikat());
        assertEquals(1, jono.getPrioriteetti());
        assertEquals(valintatapajonoOid1, jono.getValintatapajonoOid());

        assertEquals(2, jono.getJonosijat().size());
        jono.getJonosijatAsList().sort(jonosijaComparator);

        {
          Jonosija jonosija1 = jono.getJonosijatAsList().get(0);
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
          // assertNotNull(jarjestyskriteeritulos1.getHistoria());
          /*         Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());*/

          Jarjestyskriteeritulos jarjestyskriteeritulos2 =
              jonosija1.getJarjestyskriteeritulokset().get(1);
          assertEquals(new BigDecimal("200.0"), jarjestyskriteeritulos2.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
          assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
          /*          assertNotNull(jarjestyskriteeritulos2.getHistoria());
          Jarjestyskriteerihistoria historia2 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
          assertNotNull(historia2);
          assertNotNull(historia2.getHistoria());
          assertNotNull(historia2.getHistoriaGzip());*/

          assertEquals(2, jonosija1.getFunktioTulokset().funktioTulokset.size());
          assertEquals("100.0", jonosija1.getFunktioTulokset().funktioTulokset.get(0).getArvo());
          assertEquals("200.0", jonosija1.getFunktioTulokset().funktioTulokset.get(1).getArvo());
        }

        {
          Jonosija jonosija2 = jono.getJonosijatAsList().get(1);
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
          /*          assertNotNull(jarjestyskriteeritulos1.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());
          assertNotNull(historia1.getHistoriaGzip());*/

          Jarjestyskriteeritulos jarjestyskriteeritulos2 =
              jonosija2.getJarjestyskriteeritulokset().get(1);
          assertEquals(new BigDecimal("200.0"), jarjestyskriteeritulos2.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
          assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
          /*          assertNotNull(jarjestyskriteeritulos2.getHistoria());
          Jarjestyskriteerihistoria historia2 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
          assertNotNull(historia2);
          assertNotNull(historia2.getHistoria());
          assertNotNull(historia2.getHistoriaGzip());*/
        }
      }
      {
        Valintatapajono jono = valinnanvaihe1.getValintatapajono().get(1);
        assertEquals(20, jono.getAloituspaikat());
        assertEquals(2, jono.getPrioriteetti());
        assertEquals(valintatapajonoOid2, jono.getValintatapajonoOid());

        assertEquals(2, jono.getJonosijat().size());
        jono.getJonosijatAsList().sort(jonosijaComparator);

        {
          Jonosija jonosija = jono.getJonosijatAsList().get(0);
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
          /*assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia);
          assertNotNull(historia.getHistoria());*/
        }

        {
          Jonosija jonosija = jono.getJonosijatAsList().get(1);
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
          /*          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia);
          assertNotNull(historia.getHistoria());*/
        }
      }
    }

    {
      Valinnanvaihe valinnanvaihe2 = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid2);
      assertEquals(hakuOid, valinnanvaihe2.getHakuOid());
      assertEquals(hakukohdeOid1, valinnanvaihe2.getHakukohdeOid());
      assertEquals(1, valinnanvaihe2.getJarjestysnumero());
      assertEquals(valinnanvaiheOid2, valinnanvaihe2.getValinnanVaiheOid());
      assertEquals(1, valinnanvaihe2.getValintatapajono().size());

      Comparator<Jonosija> jonosijaComparator = Comparator.comparing(Jonosija::getHakemusOid);

      {
        Valintatapajono jono = valinnanvaihe2.getValintatapajono().get(0);
        assertEquals(30, jono.getAloituspaikat());
        assertEquals(1, jono.getPrioriteetti());
        assertEquals(valintatapajonoOid3, jono.getValintatapajonoOid());

        assertEquals(2, jono.getJonosijat().size());
        jono.getJonosijatAsList().sort(jonosijaComparator);

        {
          Jonosija jonosija = jono.getJonosijatAsList().get(0);
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
          /*          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());*/

          assertEquals(0, jonosija.getFunktioTulokset().funktioTulokset.size());
        }
        {
          Jonosija jonosija = jono.getJonosijatAsList().get(1);
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
          /*          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());*/
        }
      }
    }

    ////////////
    {
      Valinnanvaihe valinnanvaihe3 = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid3);
      assertEquals(hakuOid, valinnanvaihe3.getHakuOid());
      assertEquals(hakukohdeOid2, valinnanvaihe3.getHakukohdeOid());
      assertEquals(0, valinnanvaihe3.getJarjestysnumero());
      assertEquals(valinnanvaiheOid3, valinnanvaihe3.getValinnanVaiheOid());
      assertEquals(2, valinnanvaihe3.getValintatapajono().size());

      Comparator<Jonosija> jonosijaComparator = Comparator.comparing(Jonosija::getHakemusOid);

      {
        Valintatapajono jono = valinnanvaihe3.getValintatapajono().get(0);
        assertEquals(40, jono.getAloituspaikat());
        assertEquals(1, jono.getPrioriteetti());
        assertEquals(valintatapajonoOid4, jono.getValintatapajonoOid());

        assertEquals(2, jono.getJonosijat().size());
        List<Jonosija> jonot = jono.getJonosijatAsList();
        jonot.sort(jonosijaComparator);

        {
          Jonosija jonosija1 = jonot.get(0);
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
          /*          assertNotNull(jarjestyskriteeritulos1.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());*/

          Jarjestyskriteeritulos jarjestyskriteeritulos2 =
              jonosija1.getJarjestyskriteeritulokset().get(1);
          assertEquals(new BigDecimal("600.0"), jarjestyskriteeritulos2.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
          assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
          /*          assertNotNull(jarjestyskriteeritulos2.getHistoria());
          Jarjestyskriteerihistoria historia2 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
          assertNotNull(historia2);
          assertNotNull(historia2.getHistoria());*/
          assertEquals(1, jonosija1.getFunktioTulokset().funktioTulokset.size());
          assertEquals("600.0", jonosija1.getFunktioTulokset().funktioTulokset.get(0).getArvo());
        }

        {
          Jonosija jonosija2 = jonot.get(1);
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
          /*          assertNotNull(jarjestyskriteeritulos1.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos1.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());*/

          Jarjestyskriteeritulos jarjestyskriteeritulos2 =
              jonosija2.getJarjestyskriteeritulokset().get(1);
          assertEquals(new BigDecimal("600.0"), jarjestyskriteeritulos2.getArvo());
          assertEquals(
              JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA, jarjestyskriteeritulos2.getTila());
          assertEquals(2, jarjestyskriteeritulos2.getPrioriteetti());
          /*          assertNotNull(jarjestyskriteeritulos2.getHistoria());
          Jarjestyskriteerihistoria historia2 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos2.getHistoria());
          assertNotNull(historia2);
          assertNotNull(historia2.getHistoria());*/
        }
      }
      {
        Valintatapajono jono = valinnanvaihe3.getValintatapajono().get(1);
        assertEquals(50, jono.getAloituspaikat());
        assertEquals(2, jono.getPrioriteetti());
        assertEquals(valintatapajonoOid5, jono.getValintatapajonoOid());

        assertEquals(2, jono.getJonosijat().size());
        List<Jonosija> jonot = jono.getJonosijatAsList();
        jonot.sort(jonosijaComparator);

        {
          Jonosija jonosija = jonot.get(0);
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
          /*          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia);
          assertNotNull(historia.getHistoria());*/
        }

        {
          Jonosija jonosija = jonot.get(1);
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
          /*assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia);
          assertNotNull(historia.getHistoria());*/
        }
      }
    }
    {
      Valinnanvaihe valinnanvaihe4 = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid4);
      assertEquals(hakuOid, valinnanvaihe4.getHakuOid());
      assertEquals(hakukohdeOid2, valinnanvaihe4.getHakukohdeOid());
      assertEquals(1, valinnanvaihe4.getJarjestysnumero());
      assertEquals(valinnanvaiheOid4, valinnanvaihe4.getValinnanVaiheOid());
      assertEquals(1, valinnanvaihe4.getValintatapajono().size());

      Comparator<Jonosija> jonosijaComparator = Comparator.comparing(Jonosija::getHakemusOid);

      {
        Valintatapajono jono = valinnanvaihe4.getValintatapajono().get(0);
        assertEquals(60, jono.getAloituspaikat());
        assertEquals(1, jono.getPrioriteetti());
        assertEquals(valintatapajonoOid6, jono.getValintatapajonoOid());

        assertEquals(2, jono.getJonosijat().size());

        List<Jonosija> jonot = jono.getJonosijatAsList();
        jonot.sort(jonosijaComparator);

        {
          Jonosija jonosija = jonot.get(0);
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
          /*          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());*/
        }
        {
          Jonosija jonosija = jonot.get(1);
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
          /*          assertNotNull(jarjestyskriteeritulos.getHistoria());
          Jarjestyskriteerihistoria historia1 =
              jarjestyskriteerihistoriaDAO.hae(jarjestyskriteeritulos.getHistoria());
          assertNotNull(historia1);
          assertNotNull(historia1.getHistoria());*/
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

    luoEdellinenVaihe(hakemusOid, hakukohdeOid, hakuOid);

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
    assertEquals(valinnanVaiheOid, vaihe.getValinnanVaiheOid());
    assertEquals(1, vaihe.getValintatapajono().size());

    Valintatapajono jono = vaihe.getValintatapajono().get(0);
    assertEquals(valintatapajonoOid, jono.getValintatapajonoOid());
    assertEquals(1, jono.getJonosijat().size());

    Jonosija jonosija = jono.getJonosijatAsList().get(0);
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

    luoEdellinenVaihe(hakemusOid, hakukohdeOid, hakuOid);

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
    assertEquals(valinnanVaiheOid, vaihe.getValinnanVaiheOid());
    assertEquals(1, vaihe.getValintatapajono().size());

    Valintatapajono jono = vaihe.getValintatapajono().get(0);
    assertEquals(valintatapajonoOid, jono.getValintatapajonoOid());
    assertEquals(1, jono.getJonosijat().size());

    Jonosija jonosija = jono.getJonosijatAsList().get(0);
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
    final String hakijaOid = "1.2.246.562.24.43656814814", hakijaOid2 = "1.2.246.562.24.31678660760";
    final String etunimi = "Jaakko Matias", sukunimi = "Rantamäki";
    final String etunimi2 =  "Saara Sofia", sukunimi2 = "Vetelöinen";

    valinnanvaiheRepository.save(
      TestDataUtil.luoValinnanvaiheEntity(
        hakuOid,
        hakukohdeOid,
        1,
        "vv1",
        Arrays.asList(
          TestDataUtil.luoValintatapaJonoEntity(
            0,
            Set.of(luoHylattyJonosija(hakemusOid, etunimi, sukunimi, hakijaOid),
              luoHylattyJonosija(hakemusOid2, etunimi2, sukunimi2, hakijaOid2)),
            "Ammattitutkinnolla ja ulkomaisella tutkinnolla hakevat",
            0,
            Tasasijasaanto.YLITAYTTO,
            "vtpj-1"),
          TestDataUtil.luoValintatapaJonoEntity(
            245,
            Set.of(luoHylattyJonosijaValisijoittelussa(hakemusOid, etunimi, sukunimi, hakijaOid),
              luoHylattyJonosija(hakemusOid2, etunimi2, sukunimi2, hakijaOid2)),
            "Ylioppilaat ja ammatillisella perustutkinnolla hakevat",
            1,
            Tasasijasaanto.YLITAYTTO,
            "vtpj-2"))));

    luoValintakoeOsallistuminen(hakemusOid, hakukohdeOid, hakuOid, hakijaOid, false);
    luoValintakoeOsallistuminen(hakemusOid2, hakukohdeOid, hakuOid, hakijaOid2, true);

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
        vaihe.getValintatapajono().get(0).getJonosijat().stream()
            .filter(j -> j.getHakemusOid().equals(hakemusOid))
            .findFirst()
            .get()
            .getJarjestyskriteeritulokset()
            .get(0)
            .getTila());
    assertEquals(
        JarjestyskriteerituloksenTila.HYLATTY,
        vaihe.getValintatapajono().get(0).getJonosijat().stream()
            .filter(j -> j.getHakemusOid().equals(hakemusOid2))
            .findFirst()
            .get()
            .getJarjestyskriteeritulokset()
            .get(0)
            .getTila());
    assertEquals(
        "Pisteesi eivät riittäneet valintakoekutsuun",
        vaihe.getValintatapajono().get(0).getJonosijat().stream()
            .filter(j -> j.getHakemusOid().equals(hakemusOid2))
            .findFirst()
            .get()
            .getJarjestyskriteeritulokset()
            .get(0)
            .getKuvaus()
            .get("FI"));
  }

  @Test
  public void valisijoittelussaVoiTullaHyvaksytyksiVaikkaToiseltaKohteeltaEiLoytyisiKutsujaKaikkiinKohteenKokeisiin() {
    final String valinnanVaiheOid = "vv4";
    final String valintatapajonoOid = "jono2";
    final String hakemusOid = "1.2.246.562.11.00001212279";
    final String hakukohdeOid = "1.2.246.562.20.66128426039";
    final String hakuOid = "1.2.246.562.29.173465377510";
    final String etunimi = "Jaakko, Matias", sukunimi = "Rantamäki";
    final String hakijaOid = "1.2.246.562.24.43656814814";


    valinnanvaiheRepository.save(
        TestDataUtil.luoValinnanvaiheEntity(
            hakuOid,
            hakukohdeOid,
            1,
            "vv1",
            Arrays.asList(
                TestDataUtil.luoValintatapaJonoEntity(
                    0,
                    Set.of(luoHylattyJonosija(hakemusOid, etunimi, sukunimi, hakijaOid)),
                    "Ammattitutkinnolla ja ulkomaisella tutkinnolla hakevat",
                    0,
                    Tasasijasaanto.YLITAYTTO,
                    "vtpj-1"),
                TestDataUtil.luoValintatapaJonoEntity(
                    245,
                    Set.of(luoHylattyJonosijaValisijoittelussa(hakemusOid, etunimi, sukunimi, hakijaOid)),
                    "Ylioppilaat ja ammatillisella perustutkinnolla hakevat",
                    1,
                    Tasasijasaanto.YLITAYTTO,
                    "vtpj-2"))));

    luoValintakoeOsallistuminen(hakemusOid, hakukohdeOid, hakuOid, hakijaOid, false);

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
        vaihe.getValintatapajono().stream()
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

  @NotNull
  private static Jonosija luoHylattyJonosijaValisijoittelussa(String hakemusOid, String etunimi, String sukunimi, String hakijaOid) {
    Jonosija sija = TestDataUtil.luoJonosijaEntity(
      etunimi,
      sukunimi,
      hakemusOid,
      2,
      false,
      Arrays.asList(
        TestDataUtil.luoJarjestyskriteeritulosEntity(12.0, 0, HYLATTY),
        TestDataUtil.luoJarjestyskriteeritulosEntity(2.0, 1, HYVAKSYTTAVISSA),
        TestDataUtil.luoJarjestyskriteeritulosEntity(6.82, 2, HYVAKSYTTAVISSA)));

    sija.setHylattyValisijoittelussa(true);
    sija.setHakijaOid(hakijaOid);
    return sija;
  }

  @NotNull
  private static Jonosija luoHylattyJonosija(String hakemusOid, String etunimi, String sukunimi, String hakijaOid) {
    Jonosija sija = TestDataUtil.luoJonosijaEntity(
      etunimi,
      sukunimi,
      hakemusOid,
      1,
      false,
      List.of(TestDataUtil.luoJarjestyskriteeritulosEntity(0, 0, HYLATTY)));
    sija.getJarjestyskriteeritulokset().get(0).setKuvausFI("Hakemus hyväksyttiin korkeammalle hakutoiveelle");
    sija.setHakijaOid(hakijaOid);
    return sija;
  }

  private void luoValintakoeOsallistuminen(String hakemusOid, String hakukohdeOid, String hakuOid, String hakijaOid, boolean osallistumisetHylatty) {
    valintakoeOsallistuminenRepository.save(
        TestDataUtil.luoValintakoeOsallistuminen(
          hakuOid,
          hakijaOid,
          hakemusOid,
            Set.of(
                TestDataUtil.luoHakutoiveEntity(
                    "1.2.246.562.20.75182408387",
                    Set.of(
                        TestDataUtil.luoValintakoeValinnanvaiheEntity(
                            2,
                            "vv2" + hakijaOid,
                            Arrays.asList(
                                TestDataUtil.luoValintakoeEntity(
                                    "1413283036386-2122258152875806456",
                                    "SOTE1_kaikkiosiot",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"),
                                TestDataUtil.luoValintakoeEntity(
                                    "14132830363881382138007672413981",
                                    "SOTEKOE_VK_RYHMA1",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"),
                                TestDataUtil.luoValintakoeEntity(
                                    "1413373250365-957582059449313229",
                                    "kielikoe_amk_fi",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"))))),
                TestDataUtil.luoHakutoiveEntity(
                    "1.2.246.562.20.68508673735",
                    Set.of(
                        TestDataUtil.luoValintakoeValinnanvaiheEntity(
                            2,
                            "vv3" + hakijaOid,
                            Arrays.asList(
                                TestDataUtil.luoValintakoeEntity(
                                    "1413283096804-4591090746315355326",
                                    "SOTE1_kaikkiosiot",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"),
                                TestDataUtil.luoValintakoeEntity(
                                    "1413283096806-1884654158026567207",
                                    "SOTEKOE_VK_RYHMA1",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"),
                                TestDataUtil.luoValintakoeEntity(
                                    "1413373326095-2455808773394676892",
                                    "kielikoe_amk_fi",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"))))),
                TestDataUtil.luoHakutoiveEntity(
                  hakukohdeOid,
                    Set.of(
                        TestDataUtil.luoValintakoeValinnanvaiheEntity(
                            2,
                            "vv5" + hakijaOid,
                            Arrays.asList(
                                TestDataUtil.luoValintakoeEntity(
                                    "14132865785323928261359415431364",
                                    "SOTE1_kaikkiosiot",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"),
                                TestDataUtil.luoValintakoeEntity(
                                    "1413286578535-5213085081652469216",
                                    "SOTEKOE_VK_RYHMA1",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"),
                                TestDataUtil.luoValintakoeEntity(
                                    "1413286578535-5213085081652461564",
                                    "KOHTEEN_SPESIFI_KOE_BUG-1564",
                                    Osallistuminen.OSALLISTUU,
                                    null,
                                    osallistumisetHylatty ? "HYLATTY" : "HYVAKSYTTAVISSA"))))),
                TestDataUtil.luoHakutoiveEntity(
                    "1.2.246.562.20.93258129167",
                    Set.of(
                        TestDataUtil.luoValintakoeValinnanvaiheEntity(
                            1,
                            "vv6" + hakijaOid,
                            Arrays.asList(
                                TestDataUtil.luoValintakoeEntity(
                                    "1413286979019-1352529564096963601",
                                    "SOTE1_kaikkiosiot",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"),
                                TestDataUtil.luoValintakoeEntity(
                                    "1413286979022-1524554277697202959",
                                    "SOTEKOE_VK_RYHMA1",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"))))),
                TestDataUtil.luoHakutoiveEntity(
                    "1.2.246.562.20.64586301414",
                    Set.of(
                        TestDataUtil.luoValintakoeValinnanvaiheEntity(
                            2,
                            "vv7" + hakijaOid,
                            Arrays.asList(
                                TestDataUtil.luoValintakoeEntity(
                                    "1413284065430-8581021134442572546",
                                    "SOTE1_kaikkiosiot",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"),
                                TestDataUtil.luoValintakoeEntity(
                                    "14132840654327607869284614200980",
                                    "SOTEKOE_VK_RYHMA1",
                                    Osallistuminen.EI_OSALLISTU,
                                    null,
                                    "HYLATTY"))))),
                TestDataUtil.luoHakutoiveEntity(
                    "1.2.246.562.20.70883151881",
                    Set.of(
                        TestDataUtil.luoValintakoeValinnanvaiheEntity(
                            2,
                            "vv8" + hakijaOid ,
                            Arrays.asList(
                                TestDataUtil.luoValintakoeEntity(
                                    "1413284033343-6883461063685526158",
                                    "SOTE1_kaikkiosiot",
                                    Osallistuminen.OSALLISTUU,
                                    true,
                                    osallistumisetHylatty ? "HYLATTY" : "HYVAKSYTTAVISSA"),
                                TestDataUtil.luoValintakoeEntity(
                                    "1413284033345-7502478179236072968",
                                    "SOTEKOE_VK_RYHMA1",
                                    Osallistuminen.OSALLISTUU,
                                    true,
                                    osallistumisetHylatty ? "HYLATTY" : "HYVAKSYTTAVISSA"))))))));
  }

  private void luoEdellinenVaihe(String hakemusOid, String hakukohdeOid, String hakuOid) {
    Valinnanvaihe edellinen =
        TestDataUtil.luoValinnanvaiheEntity(
            hakuOid,
            hakukohdeOid,
            1,
            "1388739479946-6344111160036037403",
            List.of(
                TestDataUtil.luoValintatapaJonoEntity(
                    10,
                    Set.of(
                        TestDataUtil.luoJonosijaEntity(
                            "Valtteri",
                            "Villi",
                            "1.2.246.562.11.00000072672",
                            5,
                            false,
                            List.of(
                                TestDataUtil.luoJarjestyskriteeritulosEntity(
                                    0.0, 0, HYVAKSYTTAVISSA))),
                        TestDataUtil.luoJonosijaEntity(
                            "Keijo",
                            "Keskeyttänyt",
                            hakemusOid,
                            1,
                            false,
                            List.of(TestDataUtil.luoJarjestyskriteeritulosEntity(0.0, 0, HYLATTY))),
                        TestDataUtil.luoJonosijaEntity(
                            "Ulla",
                            "Unelias",
                            "1.2.246.562.11.00000072740",
                            3,
                            false,
                            List.of(
                                TestDataUtil.luoJarjestyskriteeritulosEntity(
                                    0.0, 0, HYVAKSYTTAVISSA)))),
                    "Harkinnanvaraisten käsittelyvaiheen valintatapajono",
                    0,
                    Tasasijasaanto.ARVONTA,
                    "1388739480159-1173947553521563587")));
    valinnanvaiheRepository.save(edellinen);
  }
}
