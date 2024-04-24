package fi.vm.sade.valintalaskenta.tulos.service.impl;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.dao.*;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

public class ValintaLaskentaTulosServiceImplTest {

  private final TulosValinnanvaiheDAO dao = Mockito.mock(TulosValinnanvaiheDAO.class);

  private final ValintalaskentaModelMapper mapper = new ValintalaskentaModelMapper();

  private final ValintalaskentaTulosService service =
      new ValintalaskentaTulosServiceImpl(
          dao,
          Mockito.mock(TulosHakijaryhmaDAO.class),
          Mockito.mock(TulosValintakoeOsallistuminenDAO.class),
          Mockito.mock(MuokattuJonosijaDAO.class),
          Mockito.mock(ValintatulosConverter.class),
          Mockito.mock(HarkinnanvarainenHyvaksyminenDAO.class),
          Mockito.mock(TulosValintatapajonoDAO.class),
          mapper,
          Mockito.mock(LaskentaAuditLog.class));

  @Test
  public void tallentaaValinnanvaiheenValintatapajonolla() {
    when(dao.haeValinnanvaihe(any())).thenReturn(null);
    ArgumentCaptor<Valinnanvaihe> vaiheCaptor = ArgumentCaptor.forClass(Valinnanvaihe.class);
    service.lisaaTuloksia(luoValinnanvaihe(), "hakukohde-1", null);
    verify(dao).saveOrUpdate(vaiheCaptor.capture());
    Valinnanvaihe captured = vaiheCaptor.getValue();
    assertNotNull(captured);
    assertEquals("valinnanvaihe", captured.getNimi());
    assertEquals("hakukohde-1", captured.getHakukohdeOid());
    assertEquals(1, captured.getValintatapajonot().size());
    assertEquals(3, captured.getValintatapajonot().get(0).getJonosijat().size());
    assertEquals(
        1,
        captured
            .getValintatapajonot()
            .get(0)
            .getJonosijat()
            .iterator()
            .next()
            .getJarjestyskriteeritulokset()
            .jarjestyskriteeritulokset
            .size());
  }

  @Test
  public void talleenOlemassaOlevanValinnanvaiheenValintatapajonolla() {
    when(dao.haeValinnanvaihe(any())).thenReturn(new Valinnanvaihe());
    ArgumentCaptor<Valinnanvaihe> vaiheCaptor = ArgumentCaptor.forClass(Valinnanvaihe.class);
    service.lisaaTuloksia(luoValinnanvaihe(), "hakukohde-1", null);
    verify(dao).saveOrUpdate(vaiheCaptor.capture());
    Valinnanvaihe captured = vaiheCaptor.getValue();
    assertNotNull(captured);
    assertEquals("hakukohde-1", captured.getHakukohdeOid());
    assertNull(captured.getNimi());
    assertEquals(1, captured.getValintatapajonot().size());
    assertEquals(3, captured.getValintatapajonot().get(0).getJonosijat().size());
    assertEquals(
        1,
        captured
            .getValintatapajonot()
            .get(0)
            .getJonosijat()
            .iterator()
            .next()
            .getJarjestyskriteeritulokset()
            .jarjestyskriteeritulokset
            .size());
  }

  private ValinnanvaiheDTO luoValinnanvaihe() {
    ValinnanvaiheDTO vaihe = new ValinnanvaiheDTO();
    vaihe.setValinnanvaiheoid("uusi-oid");
    vaihe.setJarjestysnumero(0);
    vaihe.setHakuOid("hakuoid");
    vaihe.setNimi("valinnanvaihe");
    ValintatietoValintatapajonoDTO jono = new ValintatietoValintatapajonoDTO();
    jono.setValintatapajonooid("17066814730974503524560687380688");
    jono.setOid("17066814730974503524560687380688");
    jono.setPrioriteetti(0);
    jono.setAloituspaikat(27);
    jono.setNimi("Todistusvalinta");
    jono.setSiirretaanSijoitteluun(true);
    jono.setTasasijasaanto(Tasasijasaanto.YLITAYTTO);
    jono.setEiVarasijatayttoa(false);
    jono.setPoissaOlevaTaytto(false);
    jono.setKaytetaanValintalaskentaa(false);
    jono.setValmisSijoiteltavaksi(false);
    jono.setAktiivinen(true);
    jono.setKaytetaanKokonaispisteita(false);
    jono.setVarasijat(0);
    jono.setVarasijaTayttoPaivat(0);
    jono.setJonosijat(
        List.of(
            luoJonosija(1, 1, "hakemus-1"),
            luoJonosija(2, 1, "hakemus-2"),
            luoJonosija(3, 1, "hakemus-3")));
    vaihe.setValintatapajonot(List.of(jono));
    return vaihe;
  }

  private JonosijaDTO luoJonosija(int jonosija, int prioriteetti, String hakemusoid) {
    JonosijaDTO dto = new JonosijaDTO();
    dto.setJonosija(jonosija);
    dto.setPrioriteetti(prioriteetti);
    dto.setHakemusOid(hakemusoid);
    dto.setHakijaOid(hakemusoid + "_hakija");
    dto.setTuloksenTila(JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA);
    JarjestyskriteeritulosDTO tulos = new JarjestyskriteeritulosDTO();
    tulos.setArvo(BigDecimal.valueOf(3.2));
    tulos.setPrioriteetti(0);
    tulos.setNimi("tulos");
    tulos.setKuvaus(new HashMap<>());
    dto.setJarjestyskriteerit(new TreeSet<>(Set.of(tulos)));
    return dto;
  }
}
