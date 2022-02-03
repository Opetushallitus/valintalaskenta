package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static ch.lambdaj.Lambda.having;
import static ch.lambdaj.Lambda.on;
import static com.lordofthejars.nosqlunit.core.LoadStrategyEnum.CLEAN_INSERT;
import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.EI_OSALLISTU;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.OSALLISTUU;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static java.util.stream.Collectors.groupingBy;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.hasValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import co.unruly.matchers.StreamMatchers;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;
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
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.commons.io.IOUtils;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(
    listeners = {
      DependencyInjectionTestExecutionListener.class,
      DirtiesContextTestExecutionListener.class
    })
public class ValintalaskentaResourceIntegrationTest {
  @Rule public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

  @Autowired private ApplicationContext applicationContextThatNeedsToBeAutowiredToBeIntialised;

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

  @Before
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
  @UsingDataSet(loadStrategy = LoadStrategyEnum.DELETE_ALL)
  public void
      kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydyKunValintakoevaiheitaOnYksi()
          throws JsonSyntaxException, IOException {
    LaskeDTO laskeDtoYhdenKoekutsunKanssa =
        readJson("laskeDTOYhdenKoekutsuVaiheenKanssa.json", new TypeToken<LaskeDTO>() {});
    Laskentakutsu laskentakutsu =
        new Laskentakutsu(laskeDtoYhdenKoekutsunKanssa, new SuoritustiedotDTO());
    try {
      valintalaskentaResource.toteutaLaskeKaikki(laskentakutsu);
    } catch (Exception e) {
    }

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            "1.2.246.562.29.14662042044", "1.2.246.562.11.00000003337");

    assertEquals(1, osallistuminen.getHakutoiveet().size());

    Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveet().get(0);
    ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe =
        osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(0);
    assertEquals(1, kielikokeenPakollisuusVaihe.getValintakokeet().size());

    Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(0);
    OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

    assertThat(
        kielikoetulosOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(OSALLISTUU)));
    assertThat(
        kielikoetulosOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(true)));
    assertThat(
        kielikoetulosOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));
  }

  @Test
  @UsingDataSet(loadStrategy = LoadStrategyEnum.DELETE_ALL)
  public void
      kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydyVaikkaValintakoevaiheitaOlisiUseampia()
          throws JsonSyntaxException, IOException {
    LaskeDTO laskeDtoUseammanKoekutsunKanssa =
        readJson("laskeDTOUseammanKoekutsuVaiheenKanssa.json", new TypeToken<LaskeDTO>() {});
    Laskentakutsu laskentakutsu =
        new Laskentakutsu(laskeDtoUseammanKoekutsunKanssa, new SuoritustiedotDTO());

    valintalaskentaResource.toteutaLaskeKaikki(laskentakutsu);

    ValintakoeOsallistuminen osallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            "1.2.246.562.29.14662042044", "1.2.246.562.11.00000003337");

    assertEquals(1, osallistuminen.getHakutoiveet().size());

    Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveet().get(0);
    ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe =
        osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(0);
    assertThat(kielikokeenPakollisuusVaihe.getValintakokeet(), Matchers.hasSize(2));

    Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(0);
    assertThat(
        kielikoetulos,
        having(on(Valintakoe.class).getValintakoeTunniste(), equalTo("kielikoe_fi")));
    OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();
    assertThat(
        kielikoetulosOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(OSALLISTUU)));
    assertThat(
        kielikoetulosOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(true)));
    assertThat(
        kielikoetulosOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));

    OsallistuminenTulos toisenKokeenOsallistuminenTulos =
        kielikokeenPakollisuusVaihe.getValintakokeet().get(1).getOsallistuminenTulos();
    assertThat(
        toisenKokeenOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(EI_OSALLISTU)));
    assertThat(
        toisenKokeenOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(false)));
    assertThat(
        toisenKokeenOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));

    ValintakoeValinnanvaihe paasykoeVaihe =
        osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(1);
    assertThat(paasykoeVaihe.getValintakokeet(), Matchers.hasSize(1));
    Valintakoe paasykoeTulos = paasykoeVaihe.getValintakokeet().get(0);
    assertThat(
        paasykoeTulos,
        having(
            on(Valintakoe.class).getValintakoeTunniste(),
            equalTo("1_2_246_562_20_83855523359_paasykoe")));
    OsallistuminenTulos paasykoeOsallistuminenTulos = paasykoeTulos.getOsallistuminenTulos();
    assertThat(
        paasykoeOsallistuminenTulos,
        having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(EI_OSALLISTU)));
    assertThat(
        paasykoeOsallistuminenTulos,
        having(
            on(OsallistuminenTulos.class).getKuvaus(),
            hasValue("Hylätty kielikoetulos tai ei osallistunut")));
  }

  @Test
  @UsingDataSet(locations = "bug1564.json", loadStrategy = CLEAN_INSERT)
  @Ignore(
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
    Mockito.when(
            mockValintatapajonoResource.findKopiot(org.mockito.Matchers.anyListOf(String.class)))
        .thenReturn(valisijoiteltavatJonotHakukohteittain);
    Mockito.when(
            mockValisijoitteluResource.sijoittele(
                org.mockito.Matchers.eq(hakuOid),
                org.mockito.Matchers.any(ValisijoitteluDTO.class)))
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
            .flatMap(h -> h.getValinnanVaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(ylemmankohteenValintakokeet, hasSize(2));
    assertThat(
        ylemmankohteenValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));

    Map<Osallistuminen, List<Valintakoe>> ylemmanKohteenKokeetOsallistumisenMukaan =
        ylemmankohteenValintakokeet.stream()
            .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU), hasSize(2));
    assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.keySet(), hasSize(1));
    assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.keySet(), hasItem(OSALLISTUU));
    assertThat(
        ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));

    List<Valintakoe> kohteenJossaOmaKoeValintakokeet =
        osallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOidJossaOmaKoe))
            .flatMap(h -> h.getValinnanVaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(kohteenJossaOmaKoeValintakokeet, hasSize(4));
    assertThat(
        kohteenJossaOmaKoeValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains(
            "Sote3_pakollinen_osio",
            "Sote3_valintakoe",
            "amk_kielikoe_2017_suomi",
            "mikon-testikoe-BUG-1564"));

    Map<Osallistuminen, List<Valintakoe>> kohteenJossaOmaKoeKokeetOsallistumisenMukaan =
        kohteenJossaOmaKoeValintakokeet.stream()
            .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertThat(kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(OSALLISTUU), hasSize(2));
    assertThat(
        kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(OSALLISTUU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("amk_kielikoe_2017_suomi", "mikon-testikoe-BUG-1564"));
    assertThat(kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU), hasSize(2));
    assertThat(
        kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));
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
