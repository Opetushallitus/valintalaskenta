package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.EI_OSALLISTU;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.OSALLISTUU;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static java.util.stream.Collectors.groupingBy;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import co.unruly.matchers.StreamMatchers;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintakoelaskennanKumulatiivisetTulokset;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;

public class ValintalaskentaServiceIntegrationTest extends AbstractIntegrationTest {

  @Autowired private ApplicationContext applicationContext;

  @Autowired private ValintalaskentaService valintalaskentaService;

  @Autowired private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

  private <T> T readJsonFromSamePackage(
      Class<?> clazz, String nameInSamePackage, TypeToken<T> typeToken) throws IOException {
    return new Gson()
        .fromJson(
            IOUtils.toString(
                new ClassPathResource(nameInSamePackage, clazz).getInputStream(), "UTF-8"),
            typeToken.getType());
  }

  @Test
  public void bug1564KutsuttavaKohdekohtaiseenKokeeseen() throws IOException {
    final String hakemusOidJokaKuuluuKutsua = "1.2.246.562.11.00009176948";
    final String hakemusOidJotaEiKuuluKutsua = "1.2.246.562.11.00009584734";
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

    List<HakemusDTO> kutsuttavat = new ArrayList<>();
    List<HakemusDTO> eiKutsuttavat = new ArrayList<>();
    HakemusDTO kutsuttavaHakemus =
        luoHakemus(
            hakuOid,
            hakemusOidJokaKuuluuKutsua,
            hakemusOidJokaKuuluuKutsua,
            ylempiHakukohdeOidJossaYhteinenKoe,
            hakukohdeOidJossaOmaKoe);
    kutsuttavat.add(kutsuttavaHakemus);
    HakemusDTO hakemusJotaEiKuuluKutsua =
        luoHakemus(
            hakuOid,
            hakemusOidJotaEiKuuluKutsua,
            hakemusOidJotaEiKuuluKutsua,
            ylempiHakukohdeOidJossaYhteinenKoe,
            hakukohdeOidJossaOmaKoe);
    eiKutsuttavat.add(hakemusJotaEiKuuluKutsua);
    valintalaskentaService.valintakokeetRinnakkain(
        kutsuttavat,
        perusteetKohde1,
        "uuid1",
        new ValintakoelaskennanKumulatiivisetTulokset(),
        true);
    valintalaskentaService.valintakokeetRinnakkain(
        kutsuttavat,
        perusteetKohde2,
        "uuid2",
        new ValintakoelaskennanKumulatiivisetTulokset(),
        true);
    valintalaskentaService.valintakokeetRinnakkain(
        kutsuttavat,
        perusteetKohde2,
        "uuid2",
        new ValintakoelaskennanKumulatiivisetTulokset(),
        true); // again, to get previous results in place...
    valintalaskentaService.valintakokeetRinnakkain(
        eiKutsuttavat,
        perusteetKohde1,
        "uuid1",
        new ValintakoelaskennanKumulatiivisetTulokset(),
        true);
    valintalaskentaService.valintakokeetRinnakkain(
        eiKutsuttavat,
        perusteetKohde2,
        "uuid2",
        new ValintakoelaskennanKumulatiivisetTulokset(),
        true);
    valintalaskentaService.valintakokeetRinnakkain(
        eiKutsuttavat,
        perusteetKohde2,
        "uuid2",
        new ValintakoelaskennanKumulatiivisetTulokset(),
        true); // again, to get previous results in place...

    ValintakoeOsallistuminen kutsuttavaOsallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOidJokaKuuluuKutsua);

    assertNotNull(kutsuttavaOsallistuminen);

    List<Valintakoe> kutsuttavanYlemmankohteenValintakokeet =
        kutsuttavaOsallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(ylempiHakukohdeOidJossaYhteinenKoe))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(kutsuttavanYlemmankohteenValintakokeet, hasSize(2));
    assertThat(
        kutsuttavanYlemmankohteenValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));

    Map<Osallistuminen, List<Valintakoe>> ylemmanKohteenKokeetOsallistumisenMukaan =
        kutsuttavanYlemmankohteenValintakokeet.stream()
            .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU), hasSize(2));
    assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.keySet(), hasSize(1));
    assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.keySet(), hasItem(OSALLISTUU));
    assertThat(
        ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));

    List<Valintakoe> kutsuttavanKohteenJossaOmaKoeValintakokeet =
        kutsuttavaOsallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOidJossaOmaKoe))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(kutsuttavanKohteenJossaOmaKoeValintakokeet, hasSize(4));
    assertThat(
        kutsuttavanKohteenJossaOmaKoeValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains(
            "Sote3_pakollinen_osio",
            "Sote3_valintakoe",
            "amk_kielikoe_2017_suomi",
            "mikon-testikoe-BUG-1564"));

    Map<Osallistuminen, List<Valintakoe>> kohteenJossaOmaKoeKokeetOsallistumisenMukaan =
        kutsuttavanKohteenJossaOmaKoeValintakokeet.stream()
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

    ValintakoeOsallistuminen eiKutsuttavaOsallistuminen =
        valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(
            hakuOid, hakemusOidJotaEiKuuluKutsua);

    assertNotNull(eiKutsuttavaOsallistuminen);

    List<Valintakoe> eiKutsuttavanYlemmanKohteenValintakokeet =
        eiKutsuttavaOsallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(ylempiHakukohdeOidJossaYhteinenKoe))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(eiKutsuttavanYlemmanKohteenValintakokeet, hasSize(2));
    assertThat(
        eiKutsuttavanYlemmanKohteenValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));

    Map<Osallistuminen, List<Valintakoe>> eiKutsuttavanYlemmankohteenKokeetOsallistumisenMukaan =
        eiKutsuttavanYlemmanKohteenValintakokeet.stream()
            .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertThat(
        eiKutsuttavanYlemmankohteenKokeetOsallistumisenMukaan.get(OSALLISTUU), is(nullValue()));
    assertThat(eiKutsuttavanYlemmankohteenKokeetOsallistumisenMukaan.keySet(), hasSize(1));
    assertThat(
        eiKutsuttavanYlemmankohteenKokeetOsallistumisenMukaan.keySet(), hasItem(EI_OSALLISTU));
    assertThat(
        eiKutsuttavanYlemmankohteenKokeetOsallistumisenMukaan.get(EI_OSALLISTU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));

    List<Valintakoe> eiKutsuttavanKohteenJossaOmaKoevalintakokeet =
        eiKutsuttavaOsallistuminen.getHakutoiveet().stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOidJossaOmaKoe))
            .flatMap(h -> h.getValintakoeValinnanvaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

    assertThat(eiKutsuttavanKohteenJossaOmaKoevalintakokeet, hasSize(4));
    assertThat(
        eiKutsuttavanKohteenJossaOmaKoevalintakokeet.stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains(
            "Sote3_pakollinen_osio",
            "Sote3_valintakoe",
            "amk_kielikoe_2017_suomi",
            "mikon-testikoe-BUG-1564"));

    Map<Osallistuminen, List<Valintakoe>>
        eiKutsuttavanKohteenJossaOmaKoeKokeetOsallistumisenMukaan =
            eiKutsuttavanKohteenJossaOmaKoevalintakokeet.stream()
                .collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
    assertThat(
        eiKutsuttavanKohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(OSALLISTUU), is(nullValue()));
    assertThat(
        eiKutsuttavanKohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU), hasSize(4));
    assertThat(
        eiKutsuttavanKohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU).stream()
            .map(Valintakoe::getValintakoeTunniste),
        StreamMatchers.contains(
            "Sote3_pakollinen_osio",
            "Sote3_valintakoe",
            "amk_kielikoe_2017_suomi",
            "mikon-testikoe-BUG-1564"));
  }
}
