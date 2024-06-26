package fi.vm.sade.valintalaskenta.laskenta.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.internal.verification.VerificationModeFactory.times;

import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hyvaksyttavissatila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.HakemuslaskinService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.EdellinenValinnanvaiheKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.HakemusWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.HakemuslaskinImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.JonosijaJaSyotetytArvot;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.TilaJaSelite;
import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

public class HakemuslaskinServiceTest {
  private HakemuslaskinService hakemuslaskinService;

  private LaskentaService laskentaServiceMock;
  private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAOMock;
  private EdellinenValinnanvaiheKasittelija edellinenValinnanvaiheKasittelijaMock;

  @BeforeEach
  public void setUp() {
    hakemuslaskinService = new HakemuslaskinImpl();
    laskentaServiceMock = mock(LaskentaService.class);
    jarjestyskriteerihistoriaDAOMock = mock(JarjestyskriteerihistoriaDAO.class);
    edellinenValinnanvaiheKasittelijaMock = mock(EdellinenValinnanvaiheKasittelija.class);

    ReflectionTestUtils.setField(hakemuslaskinService, "laskentaService", laskentaServiceMock);
    ReflectionTestUtils.setField(
        hakemuslaskinService, "jarjestyskriteerihistoriaDAO", jarjestyskriteerihistoriaDAOMock);
    ReflectionTestUtils.setField(
        hakemuslaskinService,
        "edellinenValinnanvaiheKasittelija",
        edellinenValinnanvaiheKasittelijaMock);
  }

  @Test
  public void test() {
    final String[] hakukohteet = {"hakukohdeOid1", "hakukohdeOid2", "hakukohdeOid3"};
    final boolean korkeakouluhaku = false;
    final Hakukohde laskettavaHakukohde =
        new Hakukohde(hakukohteet[0], new HashMap<>(), korkeakouluhaku);
    final String hakemusOid = "hakemusOid1";
    final String hakijaOid = "hakijaOid1";
    final boolean harkinnanvaraisuus = false;
    final int hakutoiveprioriteetti = 1;

    Map<String, JonosijaJaSyotetytArvot> jonosijat = new HashMap<>();

    HakemusWrapper hakemus = new HakemusWrapper();
    hakemus.setHakutoiveprioriteetti(hakutoiveprioriteetti);
    hakemus.setHakemusDTO(TestDataUtil.luoHakemus("hakuOid", hakemusOid, hakijaOid, hakukohteet));
    hakemus.setHarkinnanvaraisuus(harkinnanvaraisuus);
    hakemus.setLaskentahakemus(
        new Hakemus(hakemusOid, new HashMap<>(), new HashMap<>(), new HashMap<>()));

    final Map<String, String> edellinenValinnanvaiheTilaSelite = new HashMap<>();
    edellinenValinnanvaiheTilaSelite.put("FI", "selite");
    TilaJaSelite tilaEdellisenVaiheenMukaan =
        new TilaJaSelite(JarjestyskriteerituloksenTila.HYLATTY, edellinenValinnanvaiheTilaSelite);

    final Tila laskettuTila = new Hyvaksyttavissatila();
    final BigDecimal jarjestyskriteeriarvo = new BigDecimal("100.0");
    Laskentatulos<BigDecimal> tulos =
        new Laskentatulos<>(
            laskettuTila, jarjestyskriteeriarvo, "", new HashMap<>(), new HashMap<>());
    when(laskentaServiceMock.suoritaValintalaskenta(
            eq(laskettavaHakukohde), any(Hakemus.class),
            anyCollection(), any(Lukuarvofunktio.class)))
        .thenReturn(tulos);
    when(edellinenValinnanvaiheKasittelijaMock.tilaEdellisenValinnanvaiheenMukaan(
            eq(hakemusOid), eq(laskettuTila), any(Valinnanvaihe.class)))
        .thenReturn(tilaEdellisenVaiheenMukaan);

    hakemuslaskinService.suoritaLaskentaHakemukselle(
        laskettavaHakukohde,
        hakemus,
        new ArrayList<>(),
        mock(Lukuarvofunktio.class),
        1,
        new Valinnanvaihe(),
        jonosijat,
        "jkNimi",
        1,
        true);

    verify(jarjestyskriteerihistoriaDAOMock, times(1)).create(any(Jarjestyskriteerihistoria.class));

    assertEquals(1, jonosijat.values().size());
    Jonosija jonosija = jonosijat.values().iterator().next().getJonosija();

    assertEquals(hakemusOid, jonosija.getHakemusOid());
    assertEquals(hakijaOid, jonosija.getHakijaOid());
    assertEquals(hakutoiveprioriteetti, jonosija.getHakutoiveprioriteetti());
    assertEquals(1, jonosija.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.size());
    Jarjestyskriteeritulos jktulos =
        jonosija.getJarjestyskriteeritulokset().jarjestyskriteeritulokset.get(0);

    // Laskennan arvoa ei enää tallenneta järjestyskriteeritulokseen jos
    // hakemus on hylätty edellisessä valinnanvaiheessa
    // assertEquals(jarjestyskriteeriarvo, jktulos.getArvo());
    assertEquals(null, jktulos.getArvo());
    assertEquals(tilaEdellisenVaiheenMukaan.getTila(), jktulos.getTila());
    assertEquals(tilaEdellisenVaiheenMukaan.getSelite(), jktulos.getKuvaus());
  }
}
