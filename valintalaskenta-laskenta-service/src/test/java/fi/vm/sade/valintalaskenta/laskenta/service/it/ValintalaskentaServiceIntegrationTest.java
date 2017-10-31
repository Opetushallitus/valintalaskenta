package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static com.lordofthejars.nosqlunit.core.LoadStrategyEnum.CLEAN_INSERT;
import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.EI_OSALLISTU;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.OSALLISTUU;
import static fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil.luoHakemus;
import static java.util.stream.Collectors.groupingBy;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;

import co.unruly.matchers.StreamMatchers;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintakoelaskennanKumulatiivisetTulokset;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import org.apache.commons.io.IOUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DependencyInjectionTestExecutionListener;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(listeners = {DependencyInjectionTestExecutionListener.class,
        DirtiesContextTestExecutionListener.class})
public class ValintalaskentaServiceIntegrationTest {
    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Autowired
    private ApplicationContext applicationContext;

    @Autowired
    private ValintalaskentaService valintalaskentaService;

    @Autowired
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    private <T> T readJsonFromSamePackage(Class<?> clazz, String nameInSamePackage, TypeToken<T> typeToken) throws IOException {
        return new Gson().fromJson(IOUtils.toString(new ClassPathResource(nameInSamePackage, clazz).getInputStream(), "UTF-8"), typeToken.getType());
    }

    @Test
    @UsingDataSet(locations = "bug1564.json", loadStrategy = CLEAN_INSERT)
    public void bug1564KutsuttavaKohdekohtaiseenKokeeseen() throws IOException {
        final String hakemusOid = "1.2.246.562.11.00009176948";
        final String hakukohdeOidJossaOmaKoe = "1.2.246.562.20.80972757381";
        final String ylempiHakukohdeOidJossaYhteinenKoe = "1.2.246.562.20.68517235666";
        final String hakuOid = "1.2.246.562.29.59856749474";

        ArrayList<ValintaperusteetDTO> perusteetKohde1 = readJsonFromSamePackage(getClass(), "bug1564-valintaperusteet-ylempi.json", new TypeToken<ArrayList<ValintaperusteetDTO>>() {});
        ArrayList<ValintaperusteetDTO>  perusteetKohde2 = readJsonFromSamePackage(getClass(), "bug1564-valintaperusteet.json", new TypeToken<ArrayList<ValintaperusteetDTO>>() {});

        HakemusDTO hakemus = luoHakemus(hakuOid, hakemusOid, hakemusOid, ylempiHakukohdeOidJossaYhteinenKoe, hakukohdeOidJossaOmaKoe);
        valintalaskentaService.valintakokeet(hakemus, perusteetKohde1, "uuid1", new ValintakoelaskennanKumulatiivisetTulokset(), true);
        valintalaskentaService.valintakokeet(hakemus, perusteetKohde2, "uuid2", new ValintakoelaskennanKumulatiivisetTulokset(), true);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid(hakuOid, hakemusOid);

        assertNotNull(osallistuminen);

        List<Valintakoe> ylemmankohteenValintakokeet = osallistuminen.getHakutoiveet()
            .stream()
            .filter(h -> h.getHakukohdeOid().equals(ylempiHakukohdeOidJossaYhteinenKoe))
            .flatMap(h -> h.getValinnanVaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

        assertThat(ylemmankohteenValintakokeet, hasSize(2));
        assertThat(ylemmankohteenValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
            StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));

        Map<Osallistuminen, List<Valintakoe>> ylemmanKohteenKokeetOsallistumisenMukaan =
            ylemmankohteenValintakokeet.stream().collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
        assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU), hasSize(2));
        assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.keySet(), hasSize(1));
        assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.keySet(), hasItem(OSALLISTUU));
        assertThat(ylemmanKohteenKokeetOsallistumisenMukaan.get(OSALLISTUU).stream()
            .map(Valintakoe::getValintakoeTunniste), StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));
        
        List<Valintakoe> kohteenJossaOmaKoeValintakokeet = osallistuminen.getHakutoiveet()
            .stream()
            .filter(h -> h.getHakukohdeOid().equals(hakukohdeOidJossaOmaKoe))
            .flatMap(h -> h.getValinnanVaiheet().stream())
            .flatMap(v -> v.getValintakokeet().stream())
            .sorted(Comparator.comparing(Valintakoe::getValintakoeTunniste))
            .collect(Collectors.toList());

        assertThat(kohteenJossaOmaKoeValintakokeet, hasSize(4));
        assertThat(kohteenJossaOmaKoeValintakokeet.stream().map(Valintakoe::getValintakoeTunniste),
            StreamMatchers.contains("Sote3_pakollinen_osio","Sote3_valintakoe","amk_kielikoe_2017_suomi","mikon-testikoe-BUG-1564"));

        Map<Osallistuminen, List<Valintakoe>> kohteenJossaOmaKoeKokeetOsallistumisenMukaan =
            kohteenJossaOmaKoeValintakokeet.stream().collect(groupingBy(k -> k.getOsallistuminenTulos().getOsallistuminen()));
        assertThat(kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(OSALLISTUU), hasSize(2));
        assertThat(kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(OSALLISTUU)
            .stream()
            .map(Valintakoe::getValintakoeTunniste), StreamMatchers.contains("amk_kielikoe_2017_suomi", "mikon-testikoe-BUG-1564"));
        assertThat(kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU), hasSize(2));
        assertThat(kohteenJossaOmaKoeKokeetOsallistumisenMukaan.get(EI_OSALLISTU)
            .stream()
            .map(Valintakoe::getValintakoeTunniste), StreamMatchers.contains("Sote3_pakollinen_osio", "Sote3_valintakoe"));
    }
}
