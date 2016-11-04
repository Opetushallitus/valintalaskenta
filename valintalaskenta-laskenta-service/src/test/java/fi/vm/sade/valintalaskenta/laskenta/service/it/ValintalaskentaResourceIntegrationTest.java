package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static ch.lambdaj.Lambda.having;
import static ch.lambdaj.Lambda.on;
import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.EI_OSALLISTU;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.OSALLISTUU;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import com.lordofthejars.nosqlunit.core.LoadStrategyEnum;
import com.lordofthejars.nosqlunit.mongodb.MongoDbRule;

import fi.vm.sade.service.valintaperusteet.dto.FunktiokutsuDTO;
import fi.vm.sade.service.valintaperusteet.dto.SyoteparametriDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.Funktionimi;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
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
import org.apache.commons.io.IOUtils;
import org.hamcrest.Matchers;
import org.junit.Before;
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

@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@TestExecutionListeners(listeners = {DependencyInjectionTestExecutionListener.class,
        DirtiesContextTestExecutionListener.class})
public class ValintalaskentaResourceIntegrationTest {
    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Autowired
    private ApplicationContext applicationContextThatNeedsToBeAutowiredToBeIntialised;

    private ValintalaskentaResourceImpl valintalaskentaResource;

    @Autowired
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

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

    @Autowired
    private ValintalaskentaService valintalaskentaService;
    @Autowired
    private ValisijoitteluKasittelija valisijoitteluKasittelija;

    private ValiSijoitteluResource valisijoitteluResource = null;
    private ErillisSijoitteluResource erillisSijoitteluResource = null;
    private ValintaperusteetValintatapajonoResource valintatapajonoResource = null;

    @Before
    public void initResource() {
        valintalaskentaResource = new ValintalaskentaResourceImpl(valintalaskentaService, valisijoitteluKasittelija,
            valisijoitteluResource, erillisSijoitteluResource, valintatapajonoResource);
    }

    @Test
    @UsingDataSet(loadStrategy = LoadStrategyEnum.DELETE_ALL)
    public void kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydyKunValintakoevaiheitaOnYksi() throws JsonSyntaxException, IOException {
        LaskeDTO laskeDtoYhdenKoekutsunKanssa = readJson("laskeDTOYhdenKoekutsuVaiheenKanssa.json", new TypeToken<LaskeDTO>() {});

        valintalaskentaResource.laskeKaikki(laskeDtoYhdenKoekutsunKanssa);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid("1.2.246.562.29.14662042044", "1.2.246.562.11.00000003337");

        assertEquals(1, osallistuminen.getHakutoiveet().size());

        Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveet().get(0);
        ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe = osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(0);
        assertEquals(1, kielikokeenPakollisuusVaihe.getValintakokeet().size());

        Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(0);
        OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(OSALLISTUU)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(true)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));
    }

    @Test
    @UsingDataSet(loadStrategy = LoadStrategyEnum.DELETE_ALL)
    public void kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydyVaikkaValintakoevaiheitaOlisiUseampia() throws JsonSyntaxException, IOException {
        LaskeDTO laskeDtoUseammanKoekutsunKanssa = readJson("laskeDTOUseammanKoekutsuVaiheenKanssa.json", new TypeToken<LaskeDTO>() {});

        valintalaskentaResource.laskeKaikki(laskeDtoUseammanKoekutsunKanssa);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid("1.2.246.562.29.14662042044", "1.2.246.562.11.00000003337");

        assertEquals(1, osallistuminen.getHakutoiveet().size());

        Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveet().get(0);
        ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe = osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(0);
        assertThat(kielikokeenPakollisuusVaihe.getValintakokeet(), Matchers.hasSize(2));

        Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(0);
        assertThat(kielikoetulos, having(on(Valintakoe.class).getValintakoeTunniste(), equalTo("kielikoe_fi")));
        OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(OSALLISTUU)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(true)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));

        OsallistuminenTulos toisenKokeenOsallistuminenTulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(1).getOsallistuminenTulos();
        assertThat(toisenKokeenOsallistuminenTulos, having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(EI_OSALLISTU)));
        assertThat(toisenKokeenOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(false)));
        assertThat(toisenKokeenOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));

        ValintakoeValinnanvaihe paasykoeVaihe = osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(1);
        assertThat(paasykoeVaihe.getValintakokeet(), Matchers.hasSize(1));
        Valintakoe paasykoeTulos = paasykoeVaihe.getValintakokeet().get(0);
        assertThat(paasykoeTulos, having(on(Valintakoe.class).getValintakoeTunniste(), equalTo("1_2_246_562_20_83855523359_paasykoe")));
        OsallistuminenTulos paasykoeOsallistuminenTulos = paasykoeTulos.getOsallistuminenTulos();
        assertThat(paasykoeOsallistuminenTulos, having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(EI_OSALLISTU)));
        assertThat(paasykoeOsallistuminenTulos, having(on(OsallistuminenTulos.class).getKuvaus(), hasValue("Hylätty kielikoetulos tai ei osallistunut")));

    }

    private <T> T readJson(String pathInClasspath, TypeToken<T> typeToken) throws IOException {
        return new Gson().fromJson(IOUtils.toString(new ClassPathResource(pathInClasspath).getInputStream()), typeToken.getType());
    }
}
