package fi.vm.sade.valintalaskenta.laskenta.dao;

import static org.junit.jupiter.api.Assertions.*;

import com.google.common.collect.Sets;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.util.List;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class ValinnanvaiheDAOTest extends AbstractIntegrationTest {

  @Autowired private ValinnanvaiheDAO valinnanvaiheDAO;

  private static final String HAKUKOHDE = "hakukohdeOid1", HAKU = "hakuOid1";

  @Test
  public void testHaeEdellinenValinnanvaihe() {
    valinnanvaiheRepository.save(createValinnanvaihe(1));
    valinnanvaiheRepository.save(createValinnanvaihe(2));
    valinnanvaiheRepository.save(createValinnanvaihe(3));

    final String edellinenValinnanvaiheOid = "valinnanvaiheOid2";
    final int edellinenValinnanvaiheJarjestysnumero = 2;

    Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeEdeltavaValinnanvaihe(HAKU, HAKUKOHDE, 3);
    assertEquals(HAKU, valinnanvaihe.getHakuOid());
    assertEquals(HAKUKOHDE, valinnanvaihe.getHakukohdeOid());
    assertEquals(edellinenValinnanvaiheOid, valinnanvaihe.getValinnanVaiheOid());
    assertEquals(edellinenValinnanvaiheJarjestysnumero, valinnanvaihe.getJarjestysnumero());

    assertNull(valinnanvaiheDAO.haeEdeltavaValinnanvaihe(HAKU, HAKUKOHDE, 1));
  }

  @Test
  public void testHaeViimeisinValinnanvaihe() {
    valinnanvaiheRepository.save(createValinnanvaihe(2));
    valinnanvaiheRepository.save(createValinnanvaihe(200));

    final String edellinenValinnanvaiheOid = "valinnanvaiheOid2";
    final int edellinenValinnanvaiheJarjestysnumero = 2;

    Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeViimeisinValinnanvaihe(HAKU, HAKUKOHDE, 200);
    assertEquals(HAKU, valinnanvaihe.getHakuOid());
    assertEquals(HAKUKOHDE, valinnanvaihe.getHakukohdeOid());
    assertEquals(edellinenValinnanvaiheOid, valinnanvaihe.getValinnanVaiheOid());
    assertEquals(edellinenValinnanvaiheJarjestysnumero, valinnanvaihe.getJarjestysnumero());

    Valinnanvaihe valinnanvaihe2 = valinnanvaiheDAO.haeViimeisinValinnanvaihe(HAKU, HAKUKOHDE, 201);
    assertEquals(HAKU, valinnanvaihe2.getHakuOid());
    assertEquals(HAKUKOHDE, valinnanvaihe2.getHakukohdeOid());
    assertEquals("valinnanvaiheOid200", valinnanvaihe2.getValinnanVaiheOid());
    assertEquals(200, valinnanvaihe2.getJarjestysnumero());

    assertNull(valinnanvaiheDAO.haeViimeisinValinnanvaihe(HAKU, HAKUKOHDE, 1));
  }

  @Test
  public void testHaeValinnanvaihe() {
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
    valinnanvaihe.setValintatapajono(List.of(valintatapajono));
    valintatapajono.setJonosijat(Sets.newHashSet(new Jonosija(), new Jonosija()));
    valinnanvaihe.setValinnanVaiheOid("uusiValinnanvaiheOid");
    valinnanvaiheRepository.save(valinnanvaihe);
    assertTrue(valinnanvaiheDAO.haeValinnanvaiheLite("uusiValinnanvaiheOid").isPresent());
  }

  @Disabled
  @Test
  public void testLoadingValintatapajonoWithoutJonosijat() {
    Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe("tyhjaValinnanvaiheOid");
    valinnanvaihe
        .getValintatapajono()
        .forEach(
            valintatapajono -> {
              assertTrue(valintatapajono.getJonosijat().isEmpty());
            });
  }

  @Test
  public void testSavingAndLoadingNewValinnanvaihe() {
    Valinnanvaihe valinnanvaihe = createValinnanvaihe(1);
    Valintatapajono valintatapajono = new Valintatapajono();
    valinnanvaihe.setValintatapajono(List.of(valintatapajono));
    valintatapajono.setJonosijat(
        Sets.newHashSet(
            createJonosija("ruhtinas-nukettaja-oid"), createJonosija("kreivi-dacula-oid")));
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

  private Jonosija createJonosija(String hakijaOid) {
    Jonosija jono = new Jonosija();
    jono.setHakijaOid(hakijaOid);
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
