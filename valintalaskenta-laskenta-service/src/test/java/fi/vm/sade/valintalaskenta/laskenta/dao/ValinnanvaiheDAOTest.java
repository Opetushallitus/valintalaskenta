package fi.vm.sade.valintalaskenta.laskenta.dao;

import static org.junit.Assert.*;

import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import java.util.Arrays;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValinnanvaiheRepository;
import fi.vm.sade.valintalaskenta.laskenta.testing.AbstractIntegrationTest;
import org.hamcrest.Matchers;
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
    valinnanvaihe.valintatapajono.addAll(Arrays.asList(valintatapajono));
    valintatapajono.setJonosijat(Arrays.asList(new Jonosija(), new Jonosija()));
    valinnanvaihe.setValinnanVaiheOid("uusiValinnanvaiheOid");
    valinnanvaiheDAO.saveOrUpdate(valinnanvaihe);
    Valinnanvaihe savedValinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe("uusiValinnanvaiheOid");
    assertNotNull(savedValinnanvaihe);
    assertThat(savedValinnanvaihe.getValintatapajono().get(0).getJonosijat(), Matchers.hasSize(2));
    //assertThat(savedValinnanvaihe.getValintatapajonot().get(0).getJonosijaIdt(), Matchers.hasSize(2));
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
}
