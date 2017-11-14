package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON_TYPE;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.when;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.valinta.http.HttpResourceBuilder;
import fi.vm.sade.valintalaskenta.domain.HakukohteenLaskennanTila;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.Laskentakutsu;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija.ValisijoiteltavatJonot;
import fi.vm.sade.valintalaskenta.laskenta.testing.ValintaLaskentaLaskentaJetty;
import org.apache.commons.io.IOUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.mockito.Matchers;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;

import java.io.IOException;
import java.util.Collections;

@RunWith(JUnit4.class)
public class ValintalaskentaResourceHttpIntegrationTest {
    private ApplicationContext applicationContext;

    @Before
    public void startServer() {
        ValintaLaskentaLaskentaJetty.startShared();
        applicationContext = ValintaLaskentaLaskentaJetty.ApplicationContextGetter.applicationContext;
    }

    @Test
    @Ignore
    public void successfulLaskentaBecomesReady() throws Exception {
        when(getBean(ValisijoitteluKasittelija.class).valisijoiteltavatJonot(anyListOf(LaskeDTO.class)))
            .thenReturn(new ValisijoiteltavatJonot(Collections.emptySet(), Collections.emptyMap()));
        when(getBean(ValintalaskentaService.class).laskeKaikki(anyListOf(HakemusDTO.class), anyListOf(ValintaperusteetDTO.class),
            anyListOf(ValintaperusteetHakijaryhmaDTO.class), Matchers.eq("1.2.246.562.20.83855523359"), any(String.class), anyBoolean())).thenReturn("Onnistui!");

        LaskeDTO laskeDtoUseammanKoekutsunKanssa = readJson("laskeDTOUseammanKoekutsuVaiheenKanssa.json", new TypeToken<LaskeDTO>() {});
        Laskentakutsu laskentakutsu = new Laskentakutsu(laskeDtoUseammanKoekutsunKanssa);
        assertEquals(HakukohteenLaskennanTila.UUSI, createHttpClient("/valintalaskenta/laskekaikki").post(laskentakutsu, String.class));

        assertEquals(HakukohteenLaskennanTila.VALMIS, readStatusOf(laskentakutsu));
    }

    private String readStatusOf(Laskentakutsu laskentakutsu) {
        return createHttpClient("/valintalaskenta/status/" + laskentakutsu.getPollKey()).get(String.class);
    }

    private <T> T readJson(String pathInClasspath, TypeToken<T> typeToken) throws IOException {
        return new Gson().fromJson(readString(pathInClasspath), typeToken.getType());
    }

    private String readString(String pathInClasspath) throws IOException {
        return IOUtils.toString(new ClassPathResource(pathInClasspath).getInputStream(), "UTF-8");
    }

    private <T> T getBean(Class<T> requiredType) {
        return applicationContext.getBean(requiredType);
    }

    private WebClient createHttpClient(String path) {
        return new HttpResourceBuilder().address(ValintaLaskentaLaskentaJetty.rootUrl + path).build().getWebClient().type(APPLICATION_JSON_TYPE);
    }
}
