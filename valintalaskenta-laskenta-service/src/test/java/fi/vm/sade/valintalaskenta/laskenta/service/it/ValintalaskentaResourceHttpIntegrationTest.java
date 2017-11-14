package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON_TYPE;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.when;

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
import org.apache.cxf.jaxrs.client.WebClient;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.mockito.Matchers;
import org.springframework.context.ApplicationContext;

import java.util.Collections;

@RunWith(JUnit4.class)
public class ValintalaskentaResourceHttpIntegrationTest {
    private final String hakukohdeOid = "hakukohdeOid";
    private ApplicationContext applicationContext;

    @Before
    public void setUp() {
        ValintaLaskentaLaskentaJetty.startShared();
        applicationContext = ValintaLaskentaLaskentaJetty.ApplicationContextGetter.applicationContext;
        when(getBean(ValisijoitteluKasittelija.class).valisijoiteltavatJonot(anyListOf(LaskeDTO.class)))
            .thenReturn(new ValisijoiteltavatJonot(Collections.emptySet(), Collections.emptyMap()));
    }

    @Test
    public void successfulLaskentaBecomesReady() throws Exception {
        when(getBean(ValintalaskentaService.class).laskeKaikki(anyListOf(HakemusDTO.class), anyListOf(ValintaperusteetDTO.class),
            anyListOf(ValintaperusteetHakijaryhmaDTO.class), Matchers.eq(hakukohdeOid), any(String.class), anyBoolean())).thenReturn("Onnistui!");

        Laskentakutsu laskentakutsu = createLaskentakutsu("successfulUuid");
        assertEquals(HakukohteenLaskennanTila.UUSI,
            createHttpClient("/valintalaskenta/laskekaikki").post(laskentakutsu, String.class));

        assertEquals(HakukohteenLaskennanTila.VALMIS, readStatusOf(laskentakutsu));
    }

    @Test
    public void failingLaskentaIsRecognised() throws Exception {
        when(getBean(ValintalaskentaService.class).laskeKaikki(anyListOf(HakemusDTO.class), anyListOf(ValintaperusteetDTO.class),
            anyListOf(ValintaperusteetHakijaryhmaDTO.class), Matchers.eq(hakukohdeOid), any(String.class), anyBoolean()))
            .thenThrow(new RuntimeException(getClass().getSimpleName() + "-failure"));

        Laskentakutsu laskentakutsu = createLaskentakutsu("failingUuid");
        assertEquals(HakukohteenLaskennanTila.UUSI, createHttpClient("/valintalaskenta/laskekaikki").post(laskentakutsu, String.class));

        assertEquals(HakukohteenLaskennanTila.VIRHE, readStatusOf(laskentakutsu));
    }

    private Laskentakutsu createLaskentakutsu(String uuid) {
        return new Laskentakutsu(new LaskeDTO(uuid + System.currentTimeMillis(), false, false, "hakukohdeOid", Collections.emptyList(), Collections.emptyList()));
    }

    private String readStatusOf(Laskentakutsu laskentakutsu) {
        return createHttpClient("/valintalaskenta/status/" + laskentakutsu.getPollKey()).get(String.class);
    }

    private <T> T getBean(Class<T> requiredType) {
        return applicationContext.getBean(requiredType);
    }

    private WebClient createHttpClient(String path) {
        return new HttpResourceBuilder().address(ValintaLaskentaLaskentaJetty.rootUrl + path).build().getWebClient().type(APPLICATION_JSON_TYPE);
    }
}
