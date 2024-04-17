package fi.vm.sade.valintalaskenta.tulos.dao;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class MuokattuJonosijaDAOTest extends AbstractIntegrationTest {

  @Autowired private MuokattuJonosijaDAO dao;

  @Test
  public void eiLoydaMuokattuaJonosijaaHakuOidilla() {
    List<MuokattuJonosija> jonosijas = dao.readByHakuOid("hakuOid");
    assertTrue(jonosijas.isEmpty());
  }

  @Test
  public void loytaaMuokatunJonosijaHakuOidilla() {
    Valintatapajono jono = new Valintatapajono();
    jono.setValintatapajonoOid("valintatapajonoOid");
    jono.setPrioriteetti(1);
    Valinnanvaihe vaihe = new Valinnanvaihe();
    vaihe.setHakukohdeOid("hakukohdeOid");
    vaihe.setHakuOid("hakuOid");
    vaihe.setValinnanVaiheOid("valinnanvaiheOid");
    vaihe.setValintatapajonot(List.of(jono));
    valinnanvaiheRepository.save(vaihe);

    MuokattuJonosija muoks = new MuokattuJonosija();
    muoks.setHakuOid("hakuOid");
    muoks.setHakukohdeOid("hakukohdeOid");
    muoks.setValintatapajonoOid("valintatapajonoOid");
    muoks.setHakemusOid("hakemusOid");
    muoks.setMuokkaaja("muokkaajaOid");
    muokattuJonosijaRepository.save(muoks);

    MuokattuJonosija jonosija = dao.readByHakuOid("hakuOid").get(0);
    assertEquals("hakuOid", jonosija.getHakuOid());
    assertEquals("hakukohdeOid", jonosija.getHakukohdeOid());
    assertEquals("valintatapajonoOid", jonosija.getValintatapajonoOid());
    assertEquals("hakemusOid", jonosija.getHakemusOid());
    assertEquals("muokkaajaOid", jonosija.getMuokkaaja());
  }
}
