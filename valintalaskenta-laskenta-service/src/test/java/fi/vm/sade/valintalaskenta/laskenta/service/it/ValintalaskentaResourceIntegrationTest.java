package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.EI_OSALLISTU;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.OSALLISTUU;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static java.util.stream.Collectors.groupingBy;
import static org.junit.jupiter.api.Assertions.*;

import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import fi.vm.sade.service.valintaperusteet.dto.FunktiokutsuDTO;
import fi.vm.sade.service.valintaperusteet.dto.SyoteparametriDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import fi.vm.sade.valintalaskenta.domain.HakukohteenLaskennanTila;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.Laskentakutsu;
import fi.vm.sade.valintalaskenta.domain.dto.SuoritustiedotDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.testing.AbstractMocklessIntegrationTest;
import java.io.IOException;
import java.util.*;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;

public class ValintalaskentaResourceIntegrationTest extends AbstractMocklessIntegrationTest {

  private ValintalaskentaResourceImpl valintalaskentaResource;

  @Autowired private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

  private static final FunktiokutsuDTO totuusarvoTrue;
  private static final FunktiokutsuDTO totuusarvoFalse;

  static {
    totuusarvoTrue = new FunktiokutsuDTO();
    totuusarvoTrue.setFunktionimi(Funktionimi.TOTUUSARVO);

    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("totuusarvo");
      param.setArvo(Boolean.TRUE.toString());
      totuusarvoTrue.getSyoteparametrit().add(param);
    }

    totuusarvoFalse = new FunktiokutsuDTO();
    totuusarvoFalse.setFunktionimi(Funktionimi.TOTUUSARVO);
    {
      SyoteparametriDTO param = new SyoteparametriDTO();
      param.setAvain("totuusarvo");
      param.setArvo(Boolean.FALSE.toString());
      totuusarvoFalse.getSyoteparametrit().add(param);
    }
  }

  @Autowired private ValintalaskentaService valintalaskentaService;
  @Autowired private ValisijoitteluKasittelija valisijoitteluKasittelija;

  private ValiSijoitteluResource mockValisijoitteluResource =
      Mockito.mock(ValiSijoitteluResource.class);
  private ErillisSijoitteluResource erillisSijoitteluResource = null;
  private final ValintaperusteetValintatapajonoResource mockValintatapajonoResource =
      Mockito.mock(ValintaperusteetValintatapajonoResource.class);

  @BeforeEach
  public void initResource() {
    valintalaskentaResource =
        new ValintalaskentaResourceImpl(
            valintalaskentaService,
            valisijoitteluKasittelija,
            mockValisijoitteluResource,
            erillisSijoitteluResource,
            mockValintatapajonoResource,
            -1);
  }

  @Test
  public void
      kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydyKunValintakoevaiheitaOnYksi()
          throws JsonSyntaxException, IOException {
    LaskeDTO laskeDtoYhdenKoekutsunKanssa =
        readJson("laskeDTOYhdenKoekutsuVaiheenKanssa.json", new TypeToken<LaskeDTO>() {});
    Laskentakutsu laskentakutsu =
        new Laskentakutsu(laskeDtoYhdenKoekutsunKanssa, new SuoritustiedotDTO());
    valintalaskentaResource.toteutaLaskeKaikki(laskentakutsu);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            "1.2.246.562.29.14662042044", "1.2.246.562.11.00000003337");

    assertEquals(1, osallistuminen.getHakutoiveet().size());

    Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveetAsList().get(0);
    ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe =
        osallistumisenHakutoiveJohonOnKielikoe.getValintakoeValinnanvaiheetAsList().get(0);
    assertEquals(1, kielikokeenPakollisuusVaihe.getValintakokeet().size());

    Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeetAsList().get(0);
    OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

    assertEquals(kielikoetulosOsallistuminenTulos.getOsallistuminen(), OSALLISTUU);
    assertTrue(kielikoetulosOsallistuminenTulos.getLaskentaTulos());
    assertEquals(kielikoetulosOsallistuminenTulos.getLaskentaTila(), HYVAKSYTTAVISSA.name());
  }

  @Test
  public void
      kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydyVaikkaValintakoevaiheitaOlisiUseampia()
          throws JsonSyntaxException, IOException {
    LaskeDTO laskeDtoUseammanKoekutsunKanssa =
        readJson("laskeDTOUseammanKoekutsuVaiheenKanssa.json", new TypeToken<>() {});
    Laskentakutsu laskentakutsu =
        new Laskentakutsu(laskeDtoUseammanKoekutsunKanssa, new SuoritustiedotDTO());

    valintalaskentaResource.toteutaLaskeKaikki(laskentakutsu);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            "1.2.246.562.29.14662042044", "1.2.246.562.11.00000003337");

    assertEquals(1, osallistuminen.getHakutoiveet().size());

    Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveetAsList().get(0);
    ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe =
        osallistumisenHakutoiveJohonOnKielikoe.getValintakoeValinnanvaiheetAsList().stream()
            .filter(
                vkvv ->
                    vkvv.getValintakokeet().stream()
                        .map(Valintakoe::getValintakoeTunniste)
                        .toList()
                        .contains("kielikoe_fi"))
            .findFirst()
            .orElseThrow();
    assertEquals(2, kielikokeenPakollisuusVaihe.getValintakokeet().size());

    Valintakoe kielikoetulos =
        kielikokeenPakollisuusVaihe.getValintakokeetAsList().stream()
            .filter(vk -> vk.getValintakoeTunniste().equals("kielikoe_fi"))
            .findFirst()
            .orElseThrow();
    assertEquals("kielikoe_fi", kielikoetulos.getValintakoeTunniste());
    OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();
    assertEquals(OSALLISTUU, kielikoetulosOsallistuminenTulos.getOsallistuminen());
    assertTrue(kielikoetulosOsallistuminenTulos.getLaskentaTulos());
    assertEquals(HYVAKSYTTAVISSA.name(), kielikoetulosOsallistuminenTulos.getLaskentaTila());

    OsallistuminenTulos toisenKokeenOsallistuminenTulos =
        kielikokeenPakollisuusVaihe.getValintakokeetAsList().stream()
            .filter(vk -> !vk.getValintakoeTunniste().equals("kielikoe_fi"))
            .findFirst()
            .map(Valintakoe::getOsallistuminenTulos)
            .orElseThrow();
    assertEquals(EI_OSALLISTU, toisenKokeenOsallistuminenTulos.getOsallistuminen());
    assertFalse(toisenKokeenOsallistuminenTulos.getLaskentaTulos());
    assertEquals(HYVAKSYTTAVISSA.name(), toisenKokeenOsallistuminenTulos.getLaskentaTila());

    ValintakoeValinnanvaihe paasykoeVaihe =
        osallistumisenHakutoiveJohonOnKielikoe.getValintakoeValinnanvaiheetAsList().stream()
            .filter(
                vkvv ->
                    vkvv.getValintakokeet().stream()
                        .map(Valintakoe::getValintakoeTunniste)
                        .toList()
                        .contains("1_2_246_562_20_83855523359_paasykoe"))
            .findFirst()
            .orElse(null);
    assertEquals(1, paasykoeVaihe.getValintakokeet().size());
    Valintakoe paasykoeTulos = paasykoeVaihe.getValintakokeetAsList().get(0);
    assertEquals("1_2_246_562_20_83855523359_paasykoe", paasykoeTulos.getValintakoeTunniste());
    OsallistuminenTulos paasykoeOsallistuminenTulos = paasykoeTulos.getOsallistuminenTulos();
    assertEquals(EI_OSALLISTU, paasykoeOsallistuminenTulos.getOsallistuminen());
    assertTrue(
        paasykoeOsallistuminenTulos
            .getKuvaus()
            .containsValue("Hylätty kielikoetulos tai ei osallistunut"));
  }

  @Test
  @Disabled(
      "Valitettavasti tämä testi ei suostu menemään läpi. "
          + "Hakukohteen 1.2.246.562.20.80972757381 edellisen vaiheen jonoista ei löydy tuloksia EdellinenValinnanVaiheKasittelija :ssa.")
  public void
      kokeeseenOnKutsuttavaVaikkaEdellinenVaiheOlisiHylattyJosKoeOnVainLaskettavallaHakukohteella()
          throws IOException, InterruptedException {
    final String hakemusOid = "1.2.246.562.11.00009176948";
    final String hakukohdeOidJossaOmaKoe = "1.2.246.562.20.80972757381";
    final String ylempiHakukohdeOidJossaYhteinenKoe = "1.2.246.562.20.68517235666";
    final String hakuOid = "1.2.246.562.29.59856749474";

    ArrayList<ValintaperusteetDTO> perusteetKohde1 =
        readJsonFromSamePackage(
            getClass(),
            "bug1564-valintaperusteet-ylempi.json",
            new TypeToken<ArrayList<ValintaperusteetDTO>>() {});
    ArrayList<ValintaperusteetDTO> perusteetKohde2 =
        readJsonFromSamePackage(
            getClass(),
            "bug1564-valintaperusteet.json",
            new TypeToken<ArrayList<ValintaperusteetDTO>>() {});

    HakemusDTO hakemus =
        luoHakemus(
            hakuOid,
            hakemusOid,
            hakemusOid,
            ylempiHakukohdeOidJossaYhteinenKoe,
            hakukohdeOidJossaOmaKoe);

    Map<String, List<String>> valisijoiteltavatJonotHakukohteittain = new HashMap<>();
    valisijoiteltavatJonotHakukohteittain.put(
        ylempiHakukohdeOidJossaYhteinenKoe,
        Arrays.asList("14871500433118662321562230733453", "1487149910926-5198215752602272243"));
    valisijoiteltavatJonotHakukohteittain.put(
        hakukohdeOidJossaOmaKoe, Collections.singletonList("14871499108842038936225826786200"));
    Mockito.when(mockValintatapajonoResource.findKopiot(org.mockito.ArgumentMatchers.anyList()))
        .thenReturn(valisijoiteltavatJonotHakukohteittain);
    Mockito.when(
            mockValisijoitteluResource.sijoittele(
                org.mockito.ArgumentMatchers.eq(hakuOid),
                org.mockito.ArgumentMatchers.any(ValisijoitteluDTO.class)))
        .thenReturn(
            readJsonFromSamePackage(
                getClass(),
                "bug1564-valisijoittelu-tulos.json",
                new TypeToken<List<HakukohdeDTO>>() {}));

    LaskeDTO laskeDto1 =
        new LaskeDTO(
            "uuid1",
            true,
            false,
            ylempiHakukohdeOidJossaYhteinenKoe,
            Collections.singletonList(hakemus),
            perusteetKohde1);
    LaskeDTO laskeDto2 =
        new LaskeDTO(
            "uuid2",
            true,
            false,
            hakukohdeOidJossaOmaKoe,
            Collections.singletonList(hakemus),
            perusteetKohde2);
    String returnValue;
    do {
      returnValue =
          valintalaskentaResource.laskeJaSijoittele(
              new Laskentakutsu(Arrays.asList(laskeDto1, laskeDto2), new SuoritustiedotDTO()));
      Thread.sleep(50);
    } while (!(returnValue.equals(HakukohteenLaskennanTila.VALMIS)
        || returnValue.equals(HakukohteenLaskennanTila.VIRHE)));

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);
    assertNotNull(osallistuminen);

    List<Valintakoe> ylemmankohteenValintakokeet =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(ylempiHakukohdeOidJossaYhteinenKoe))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .toList();

    assertEquals(2, ylemmankohteenValintakokeet.size());
    assertTrue(
        ylemmankohteenValintakokeet.stream()
            .map(Valintakoe::getValintakoeTunniste)
            .toList()
            .containsAll(Set.of("Sote3_pakollinen_osio", "Sote3_valintakoe")));

    Map<Osallistuminen, List<Valintakoe>> ylemmanKohteenKokeetOsallistumisenMukaan =
        ylemmankohteenValintakokeet.stream()
            .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertEquals(2, ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU).size());
    assertEquals(1, ylemmanKohteenKokeetOsallistumisenMukaan.keySet().size());
    assertTrue(ylemmanKohteenKokeetOsallistumisenMukaan.containsKey(OSALLISTUU));
    assertTrue(
        ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU).stream()
            .map(Valintakoe::getValintakoeTunniste)
            .toList()
            .containsAll(Set.of("Sote3_pakollinen_osio", "Sote3_valintakoe")));

    List<Valintakoe> kohteenJossaOmaKoeValintakokeet =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOidJossaOmaKoe))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .toList();

    assertEquals(4, kohteenJossaOmaKoeValintakokeet.size());
    assertTrue(
        kohteenJossaOmaKoeValintakokeet.stream()
            .map(Valintakoe::getValintakoeTunniste)
            .toList()
            .containsAll(
                Set.of(
                    "Sote3_pakollinen_osio",
                    "Sote3_valintakoe",
                    "amk_kielikoe_2017_suomi",
                    "mikon-testikoe-BUG-1564")));

    Map<Osallistuminen, List<Valintakoe>> kohteenJossaOmaKoeKokeetOsallistumisenMukaan =
        kohteenJossaOmaKoeValintakokeet.stream()
            .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertEquals(2, kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(OSALLISTUU).size());
    assertTrue(
        kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(OSALLISTUU).stream()
            .map(Valintakoe::getValintakoeTunniste)
            .toList()
            .containsAll(Set.of("amk_kielikoe_2017_suomi", "mikon-testikoe-BUG-1564")));
    assertEquals(2, kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU).size());
    assertTrue(
        kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU).stream()
            .map(Valintakoe::getValintakoeTunniste)
            .toList()
            .containsAll(Set.of("Sote3_pakollinen_osio", "Sote3_valintakoe")));
  }

  private <T> T readJson(String pathInClasspath, TypeToken<T> typeToken) throws IOException {
    return new Gson()
        .fromJson(
            IOUtils.toString(new ClassPathResource(pathInClasspath).getInputStream()),
            typeToken.getType());
  }

  private <T> T readJsonFromSamePackage(
      Class<?> clazz, String nameInSamePackage, TypeToken<T> typeToken) throws IOException {
    return new Gson()
        .fromJson(
            IOUtils.toString(
                new ClassPathResource(nameInSamePackage, clazz).getInputStream(), "UTF-8"),
            typeToken.getType());
  }
}
