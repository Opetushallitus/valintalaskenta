package fi.vm.sade.valintalaskenta.tulos.dao;

import static fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi.LUKUARVO;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.*;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import fi.vm.sade.service.valintaperusteet.dto.SyoteparametriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetFunktiokutsuDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class TulosJarjestyskriteerihistoriaDAOTest extends AbstractIntegrationTest {
  @Autowired private TulosJarjestyskriteerihistoriaDAO jonosijaHistoriaTulosDAO;

  @Autowired private ValintalaskentaSuorittajaService valintalaskentaSuorittajaService;

  private static final ValintaperusteetFunktiokutsuDTO sata;

  private static final ValintaperusteetFunktiokutsuDTO kymmenen;

  private static final ValintaperusteetFunktiokutsuDTO viisi;

  static {
    sata = new ValintaperusteetFunktiokutsuDTO();
    sata.setFunktionimi(LUKUARVO);
    sata.setTallennaTulos(true);
    sata.setTulosTunniste("sata");

    SyoteparametriDTO param = new SyoteparametriDTO();
    param.setAvain("luku");
    param.setArvo("100.0");
    sata.getSyoteparametrit().add(param);

    kymmenen = new ValintaperusteetFunktiokutsuDTO();
    kymmenen.setFunktionimi(LUKUARVO);
    kymmenen.setTallennaTulos(true);
    kymmenen.setTulosTunniste("kymmenen");

    param = new SyoteparametriDTO();
    param.setAvain("luku");
    param.setArvo("10.0");
    kymmenen.getSyoteparametrit().add(param);

    viisi = new ValintaperusteetFunktiokutsuDTO();
    viisi.setFunktionimi(LUKUARVO);
    viisi.setTallennaTulos(true);
    viisi.setTulosTunniste("viisi");

    param = new SyoteparametriDTO();
    param.setAvain("luku");
    param.setArvo("5.0");
    viisi.getSyoteparametrit().add(param);
  }

  @Test
  public void findIdsByValintapajonoOidHakemusOid() {
    final String hakemusOid = "1.2.246.562.11.00000072753";
    final String valintatapajonoOid = "jono1";

    teeLaskenta(hakemusOid, valintatapajonoOid);

    List<UUID> jonosijaHistoriat =
        jonosijaHistoriaTulosDAO.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid);

    assertEquals(3, jonosijaHistoriat.size());
  }

  @Test
  public void findHistoriat() {
    String hakemusOid = "1.2.246.562.11.00000072752";
    String valintatapajonoOid = "jono2";

    teeLaskenta(hakemusOid, valintatapajonoOid);

    List<UUID> tunnisteet =
        jonosijaHistoriaTulosDAO.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid);

    List<String> historiat =
        jonosijaHistoriaTulosDAO.findByTunnisteet(tunnisteet).stream()
            .map(Jarjestyskriteerihistoria::getHistoria)
            .toList();

    assertEquals(3, historiat.size());
    assertTrue(
        historiat.contains(
            "{\"funktio\":\"Laskenta hakemukselle (1.2.246.562.11.00000072752)\",\"tulos\":100,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}],\"historiat\":[{\"funktio\":\"Lukuarvo\",\"tulos\":100,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}]}],\"avaimet\":{\"hakukohdeKutsunKohde2\":\"hakukohdeOid2\"}}"));
    assertTrue(
        historiat.contains(
            "{\"funktio\":\"Laskenta hakemukselle (1.2.246.562.11.00000072752)\",\"tulos\":10,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}],\"historiat\":[{\"funktio\":\"Lukuarvo\",\"tulos\":10,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}]}],\"avaimet\":{\"hakukohdeKutsunKohde2\":\"hakukohdeOid2\"}}"));
    assertTrue(
        historiat.contains(
            "{\"funktio\":\"Laskenta hakemukselle (1.2.246.562.11.00000072752)\",\"tulos\":5,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}],\"historiat\":[{\"funktio\":\"Lukuarvo\",\"tulos\":5,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}]}],\"avaimet\":{\"hakukohdeKutsunKohde2\":\"hakukohdeOid2\"}}"));
  }

  @Test
  public void historiatAreOrderedByPrioriteetti() {
    String hakemusOid = "1.2.246.562.11.00000072752";
    String valintatapajonoOid = "jono2";

    teeLaskenta(hakemusOid, valintatapajonoOid);

    List<UUID> tunnisteet =
        jonosijaHistoriaTulosDAO.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid);

    List<String> historiat =
        jonosijaHistoriaTulosDAO.findByTunnisteet(tunnisteet).stream()
            .map(Jarjestyskriteerihistoria::getHistoria)
            .toList();

    assertEquals(3, historiat.size());
    assertEquals(
        "{\"funktio\":\"Laskenta hakemukselle (1.2.246.562.11.00000072752)\",\"tulos\":5,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}],\"historiat\":[{\"funktio\":\"Lukuarvo\",\"tulos\":5,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}]}],\"avaimet\":{\"hakukohdeKutsunKohde2\":\"hakukohdeOid2\"}}",
        historiat.get(0));
    assertEquals(
        "{\"funktio\":\"Laskenta hakemukselle (1.2.246.562.11.00000072752)\",\"tulos\":10,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}],\"historiat\":[{\"funktio\":\"Lukuarvo\",\"tulos\":10,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}]}],\"avaimet\":{\"hakukohdeKutsunKohde2\":\"hakukohdeOid2\"}}",
        historiat.get(1));
    assertEquals(
        "{\"funktio\":\"Laskenta hakemukselle (1.2.246.562.11.00000072752)\",\"tulos\":100,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}],\"historiat\":[{\"funktio\":\"Lukuarvo\",\"tulos\":100,\"tilat\":[{\"tilatyyppi\":\"HYVAKSYTTAVISSA\"}]}],\"avaimet\":{\"hakukohdeKutsunKohde2\":\"hakukohdeOid2\"}}",
        historiat.get(2));
  }

  private void teeLaskenta(String hakemusOid, String valintatapajonoOid) {
    ValintaperusteetDTO vv3 =
        luoValintaperusteetJaTavallinenValinnanvaihe(
            "1.2.246.562.5.2013080813081926341927", "1.2.246.562.5.91937845484", "vv3", 0);
    (vv3.getValinnanVaihe())
        .getValintatapajono()
        .add(
            luoValintatapajono(
                valintatapajonoOid,
                0,
                10,
                luoJarjestyskriteeri(sata, 2),
                luoJarjestyskriteeri(viisi, 0),
                luoJarjestyskriteeri(kymmenen, 1)));
    valintalaskentaSuorittajaService.suoritaLaskenta(
        Collections.singletonList(
            luoHakemus(
                "1.2.246.562.5.2013080813081926341927",
                hakemusOid,
                hakemusOid,
                "1.2.246.562.5.91937845484")),
        Collections.singletonList(vv3),
        new ArrayList<>(),
        "1.2.246.562.5.91937845484",
        null,
        false);
  }
}
