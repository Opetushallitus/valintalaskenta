package fi.vm.sade.valintalaskenta.laskenta.dao;

import static org.junit.Assert.*;

import com.google.common.collect.Sets;
import fi.vm.sade.valintalaskenta.domain.valinta.*;

import java.util.Arrays;
import java.util.List;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValinnanvaiheRepository;
import fi.vm.sade.valintalaskenta.laskenta.testing.AbstractIntegrationTest;
import org.hamcrest.Matchers;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;

public class ValinnanvaiheDAOTest extends AbstractIntegrationTest {

  @Autowired private ValinnanvaiheDAO valinnanvaiheDAO;

  private static final String HAKUKOHDE = "hakukohdeOid1", HAKU = "hakuOid1";

  @Test
  public void testHaeEdellinenValinnanvaihe() throws InterruptedException {
    valinnanvaiheRepository.save(createValinnanvaihe(1));
    valinnanvaiheRepository.save(createValinnanvaihe(2));
    valinnanvaiheRepository.save(createValinnanvaihe(3));

    final String edellinenValinnanvaiheOid = "valinnanvaiheOid2";
    final int edellinenValinnanvaiheJarjestysnumero = 2;

    Valinnanvaihe valinnanvaihe =
        valinnanvaiheDAO.haeEdeltavaValinnanvaihe(HAKU, HAKUKOHDE, 3);
    assertEquals(HAKU, valinnanvaihe.getHakuOid());
    assertEquals(HAKUKOHDE, valinnanvaihe.getHakukohdeOid());
    assertEquals(edellinenValinnanvaiheOid, valinnanvaihe.getValinnanVaiheOid());
    assertEquals(edellinenValinnanvaiheJarjestysnumero, valinnanvaihe.getJarjestysnumero());

    assertNull(valinnanvaiheDAO.haeEdeltavaValinnanvaihe(HAKU, HAKUKOHDE, 1));
  }

  @Test
  public void testHaeValinnanvaihe() throws InterruptedException {
    valinnanvaiheRepository.save(createValinnanvaihe(1));
    valinnanvaiheRepository.save(createValinnanvaihe(2));

    final String valinnanvaiheOid = "valinnanvaiheOid2";

    Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid);
    assertEquals(valinnanvaiheOid, valinnanvaihe.getValinnanVaiheOid());
  }

  @Test
  public void testLoadingValintapajonoLite() {
    Valinnanvaihe valinnanvaihe = createValinnanvaihe(1);
    Valintatapajono valintatapajono = new Valintatapajono();
    valinnanvaihe.valintatapajono.addAll(Arrays.asList(valintatapajono));
    valintatapajono.setJonosijat(Sets.newHashSet(new Jonosija(), new Jonosija()));
    valinnanvaihe.setValinnanVaiheOid("uusiValinnanvaiheOid");
    valinnanvaiheRepository.save(valinnanvaihe);
    assertTrue(valinnanvaiheDAO.haeValinnanvaiheLite("uusiValinnanvaiheOid").isPresent());
  }

  @Ignore
  @Test
  public void testLoadingValintatapajonoWithoutJonosijat() {
    Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe("tyhjaValinnanvaiheOid");
    valinnanvaihe
        .getValintatapajono()
        .forEach(
            valintatapajono -> {
              assertThat(valintatapajono.getJonosijat(), Matchers.empty());
              //TODO: Necessary?
              //assertThat(valintatapajono.getJonosijaIdt(), Matchers.empty());
            });
  }

  @Test
  public void testSavingAndLoadingNewValinnanvaihe() {
    Valinnanvaihe valinnanvaihe = createValinnanvaihe(1);
    Valintatapajono valintatapajono = new Valintatapajono();
    valinnanvaihe.valintatapajono.addAll(List.of(valintatapajono));
    valintatapajono.setJonosijat(Sets.newHashSet(createJonosija(), new Jonosija()));
    valinnanvaihe.setValinnanVaiheOid("uusiValinnanvaiheOid");
    valinnanvaiheDAO.saveOrUpdate(valinnanvaihe);
    Valinnanvaihe savedValinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe("uusiValinnanvaiheOid");
    assertNotNull(savedValinnanvaihe);
    assertEquals(2, savedValinnanvaihe.getValintatapajono().get(0).getJonosijat().size());
    Jonosija jono = savedValinnanvaihe.getValintatapajono().get(0).getJonosijatAsList().get(0);
    FunktioTulos funkkari = jono.getFunktioTulokset().funktioTulokset.get(0);
    assertEquals("arvokas", funkkari.getArvo());
    assertEquals("arvoton", funkkari.getTunniste());
    assertEquals("Funktiotulos", funkkari.getNimiFi());
    assertEquals("Function", funkkari.getNimiEn());
    assertEquals("Funktion", funkkari.getNimiSv());
    assertTrue(funkkari.isOmaopintopolku());
    SyotettyArvo arvo = jono.getSyotetytArvot().get(0);
    assertEquals("Kultaa", arvo.getArvo());
    assertEquals("Hopeaa", arvo.getTunniste());
    assertEquals("Katinkultaa", arvo.getLaskennallinenArvo());
  }

  private Valinnanvaihe createValinnanvaihe(int jarjestysnro) {
    Valinnanvaihe vv = new Valinnanvaihe();
    vv.setHakukohdeOid(HAKUKOHDE);
    vv.setHakuOid(HAKU);
    vv.setTarjoajaOid("tarjoajaOid1");
    vv.setJarjestysnumero(jarjestysnro);
    vv.setValinnanVaiheOid("valinnanvaiheOid" + jarjestysnro);
    return vv;
  }

  private Jonosija createJonosija() {
    Jonosija jono = new Jonosija();
    jono.setFunktioTulokset(new FunktioTulosContainer(List.of(createFunktioTulos())));
    jono.setSyotetytArvot(new SyotettyArvoContainer(List.of(createSyotettyArvo())));
    return jono;
  }

  private FunktioTulos createFunktioTulos() {
    FunktioTulos tulos = new FunktioTulos();
    tulos.setArvo("arvokas");
    tulos.setTunniste("arvoton");
    tulos.setOmaopintopolku(true);
    tulos.setNimiFi("Funktiotulos");
    tulos.setNimiEn("Function");
    tulos.setNimiSv("Funktion");
    return tulos;
  }

  private SyotettyArvo createSyotettyArvo() {
    SyotettyArvo arvo = new SyotettyArvo();
    arvo.setArvo("Kultaa");
    arvo.setTunniste("Hopeaa");
    arvo.setLaskennallinenArvo("Katinkultaa");
    return arvo;
  }
}
