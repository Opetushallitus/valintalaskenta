package fi.vm.sade.valintalaskenta.laskenta.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.laskenta.Totuusarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Laskentatulos;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.ArvokonvertointiVirhe;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hylattytila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Hyvaksyttavissatila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.PakollinenValintaperusteHylkays;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Tila;
import fi.vm.sade.service.valintaperusteet.laskenta.api.tila.Virhetila;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.LaskentadomainkonvertteriWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.Valintakoeosallistumislaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.ValintakoeosallistumislaskinImpl;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/** User: wuoti Date: 6.5.2013 Time: 9.59 */
public class ValintakoeosallistumislaskinTest {

  private Valintakoeosallistumislaskin valintakoeosallistumislaskin;

  private LaskentaService laskentaServiceMock;
  private LaskentadomainkonvertteriWrapper laskentadomainkonvertteriWrapperMock;

  private final boolean korkeakouluhaku = false;

  private Map<String, String> suomenkielinenMap(String teksti) {
    Map<String, String> vastaus = new HashMap<>();
    vastaus.put("FI", teksti);
    return vastaus;
  }

  @BeforeAll
  public void setUpt() {
    laskentaServiceMock = mock(LaskentaService.class);
    laskentadomainkonvertteriWrapperMock = mock(LaskentadomainkonvertteriWrapper.class);

    valintakoeosallistumislaskin =
        new ValintakoeosallistumislaskinImpl(
            laskentaServiceMock, laskentadomainkonvertteriWrapperMock);
  }

  private void valmisteleStubit(final Hakukohde hakukohde, Tila tila, boolean tulos) {

    when(laskentadomainkonvertteriWrapperMock.muodostaTotuusarvolasku(any()))
        .thenReturn(mock(Totuusarvofunktio.class));

    final Laskentatulos<Boolean> tulos1 =
        new Laskentatulos<>(tila, tulos, "", new HashMap<>(), new HashMap<>());
    when(laskentaServiceMock.suoritaValintakoelaskenta(
            eq(hakukohde), any(Hakemus.class), any(Totuusarvofunktio.class)))
        .thenReturn(tulos1);
  }

  private Hakemus emptyHakemus() {
    return new Hakemus("hakemusOid", new HashMap<>(), new HashMap<>(), new HashMap<>());
  }

  private Funktiokutsu getKutsu() {
    Funktiokutsu funktiokutsu = new Funktiokutsu();
    funktiokutsu.setFunktionimi(Funktionimi.TOTUUSARVO);
    return funktiokutsu;
  }

  @Test
  public void testTilaHyvaksyttavissaTrue() {
    final String hakukohdeOid = "hakukohdeOid1";
    final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<>(), korkeakouluhaku);

    valmisteleStubit(hakukohde, new Hyvaksyttavissatila(), true);

    OsallistuminenTulos osallistuminen =
        valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
            hakukohde, emptyHakemus(), getKutsu());
    assertEquals(Osallistuminen.OSALLISTUU, osallistuminen.getOsallistuminen());
  }

  @Test
  public void testTilaHyvaksyttavissaFalse() {
    final String hakukohdeOid = "hakukohdeOid1";
    final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<>(), korkeakouluhaku);

    valmisteleStubit(hakukohde, new Hyvaksyttavissatila(), false);
    OsallistuminenTulos osallistuminen =
        valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
            hakukohde, emptyHakemus(), getKutsu());
    assertEquals(Osallistuminen.EI_OSALLISTU, osallistuminen.getOsallistuminen());
  }

  @Test
  public void testTilaHylattyFalse() {
    final String hakukohdeOid = "hakukohdeOid1";
    final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<>(), korkeakouluhaku);

    valmisteleStubit(
        hakukohde,
        new Hylattytila(suomenkielinenMap("kuvaus"), new PakollinenValintaperusteHylkays("")),
        false);
    OsallistuminenTulos osallistuminen =
        valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
            hakukohde, emptyHakemus(), getKutsu());
    assertEquals(Osallistuminen.OSALLISTUU, osallistuminen.getOsallistuminen());
  }

  @Test
  public void testTilaHylattyTrue() {
    final String hakukohdeOid = "hakukohdeOid1";
    final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<>(), korkeakouluhaku);

    valmisteleStubit(
        hakukohde,
        new Hylattytila(suomenkielinenMap("kuvaus"), new PakollinenValintaperusteHylkays("")),
        true);
    OsallistuminenTulos osallistuminen =
        valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
            hakukohde, emptyHakemus(), getKutsu());
    assertEquals(Osallistuminen.OSALLISTUU, osallistuminen.getOsallistuminen());
  }

  @Test
  public void testTilaVirheTrue() {
    final String hakukohdeOid = "hakukohdeOid1";
    final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<>(), korkeakouluhaku);

    valmisteleStubit(
        hakukohde, new Virhetila(suomenkielinenMap("kuvaus"), new ArvokonvertointiVirhe("")), true);
    OsallistuminenTulos osallistuminen =
        valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
            hakukohde, emptyHakemus(), getKutsu());
    assertEquals(Osallistuminen.VIRHE, osallistuminen.getOsallistuminen());
  }

  @Test
  public void testTilaVirheFalse() {
    final String hakukohdeOid = "hakukohdeOid1";
    final Hakukohde hakukohde = new Hakukohde(hakukohdeOid, new HashMap<>(), korkeakouluhaku);

    valmisteleStubit(
        hakukohde,
        new Virhetila(suomenkielinenMap("kuvaus"), new ArvokonvertointiVirhe("")),
        false);
    OsallistuminenTulos osallistuminen =
        valintakoeosallistumislaskin.laskeOsallistuminenYhdelleHakukohteelle(
            hakukohde, emptyHakemus(), getKutsu());
    assertEquals(Osallistuminen.VIRHE, osallistuminen.getOsallistuminen());
  }
}
