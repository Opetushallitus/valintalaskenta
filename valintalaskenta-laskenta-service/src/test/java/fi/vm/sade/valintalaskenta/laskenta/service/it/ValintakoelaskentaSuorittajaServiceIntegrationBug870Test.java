package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static ch.lambdaj.Lambda.having;
import static ch.lambdaj.Lambda.on;
import static com.lordofthejars.nosqlunit.mongodb.MongoDbRule.MongoDbRuleBuilder.newMongoDbRule;
import static fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila.HYVAKSYTTAVISSA;
import static fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen.OSALLISTUU;
import static org.hamcrest.Matchers.equalTo;
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
import fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.ValintakoelaskentaSuorittajaService;
import org.apache.commons.io.IOUtils;
import org.junit.Ignore;
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
public class ValintakoelaskentaSuorittajaServiceIntegrationBug870Test {
    private final String uuid = null;
    @Rule
    public MongoDbRule mongoDbRule = newMongoDbRule().defaultSpringMongoDb("test");

    @Autowired
    private ApplicationContext applicationContextThatNeedsToBeAutowiredToBeIntialised;

    @Autowired
    private ValintakoelaskentaSuorittajaService valintakoelaskentaSuorittajaService;

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

    @Test
    @UsingDataSet(loadStrategy = LoadStrategyEnum.DELETE_ALL)
    public void kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydyKunValintakoevaiheitaOnYksi() throws JsonSyntaxException, IOException {
        LaskeDTO laskeDtoYhdenKoekutsunKanssa = readJson("laskeDTOYhdenKoekutsuVaiheenKanssa.json", new TypeToken<LaskeDTO>() {});

        valintakoelaskentaSuorittajaService.laske(laskeDtoYhdenKoekutsunKanssa.getHakemus().get(0), laskeDtoYhdenKoekutsunKanssa.getValintaperuste(), uuid);

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
    @Ignore // TODO fix BUG-870
    public void kielikokeeseenKutsutaanJosSuoritustaTaiTodennettuaKielitaitoaEiLoydyVaikkaValintakoevaiheitaOlisiUseampia() throws JsonSyntaxException, IOException {
        LaskeDTO laskeDtoUseammanKoekutsunKanssa = readJson("laskeDTOUseammanKoekutsuVaiheenKanssa.json", new TypeToken<LaskeDTO>() {});

        valintakoelaskentaSuorittajaService.laske(laskeDtoUseammanKoekutsunKanssa.getHakemus().get(0), laskeDtoUseammanKoekutsunKanssa.getValintaperuste(), uuid);

        ValintakoeOsallistuminen osallistuminen = valintakoeOsallistuminenDAO.readByHakuOidAndHakemusOid("1.2.246.562.29.14662042044", "1.2.246.562.11.00000003337");

        assertEquals(1, osallistuminen.getHakutoiveet().size());

        Hakutoive osallistumisenHakutoiveJohonOnKielikoe = osallistuminen.getHakutoiveet().get(0);
        ValintakoeValinnanvaihe kielikokeenPakollisuusVaihe = osallistumisenHakutoiveJohonOnKielikoe.getValinnanVaiheet().get(0);
        assertEquals(2, kielikokeenPakollisuusVaihe.getValintakokeet().size());

        Valintakoe kielikoetulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(0);
        OsallistuminenTulos kielikoetulosOsallistuminenTulos = kielikoetulos.getOsallistuminenTulos();

        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(OSALLISTUU)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(true)));
        assertThat(kielikoetulosOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));

        OsallistuminenTulos toisenKokeenOsallistuminenTulos = kielikokeenPakollisuusVaihe.getValintakokeet().get(1).getOsallistuminenTulos();
        assertThat(toisenKokeenOsallistuminenTulos, having(on(OsallistuminenTulos.class).getOsallistuminen(), equalTo(OSALLISTUU)));
        assertThat(toisenKokeenOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTulos(), equalTo(true)));
        assertThat(toisenKokeenOsallistuminenTulos, having(on(OsallistuminenTulos.class).getLaskentaTila(), equalTo(HYVAKSYTTAVISSA.name())));
    }

    private <T> T readJson(String pathInClasspath, TypeToken<T> typeToken) throws IOException {
        return new Gson().fromJson(IOUtils.toString(new ClassPathResource(pathInClasspath).getInputStream()), typeToken.getType());
    }
}
