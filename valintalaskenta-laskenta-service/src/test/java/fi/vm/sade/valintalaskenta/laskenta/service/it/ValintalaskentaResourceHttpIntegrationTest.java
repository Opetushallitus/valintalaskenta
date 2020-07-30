package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON_TYPE;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.when;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.valinta.sharedutils.http.HttpResourceBuilder;
import fi.vm.sade.valintalaskenta.domain.HakukohteenLaskennanTila;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.Laskentakutsu;
import fi.vm.sade.valintalaskenta.domain.dto.SuoritustiedotDTO;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija.ValisijoiteltavatJonot;
import fi.vm.sade.valintalaskenta.laskenta.testing.ValintaLaskentaLaskentaJettyForTesting;
import java.util.Collections;
import org.apache.cxf.jaxrs.client.WebClient;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.mockito.Matchers;
import org.springframework.context.ApplicationContext;

@RunWith(JUnit4.class)
public class ValintalaskentaResourceHttpIntegrationTest {
  private final String hakukohdeOid = "hakukohdeOid";
  private ApplicationContext applicationContext;
  private boolean mayFinish;
  private boolean finished;

  @Before
  public void setUp() {
    ValintaLaskentaLaskentaJettyForTesting.startShared();
    applicationContext =
        ValintaLaskentaLaskentaJettyForTesting.ApplicationContextGetter.applicationContext;
  }

  @Test
  public void successfulLaskentaBecomesReady() throws Exception {
    mockValisijoiteltavatJonotCall();
    when(getBean(ValintalaskentaService.class)
            .laskeKaikki(
                anyListOf(HakemusDTO.class),
                anyListOf(ValintaperusteetDTO.class),
                anyListOf(ValintaperusteetHakijaryhmaDTO.class),
                Matchers.eq(hakukohdeOid),
                any(String.class),
                anyBoolean()))
        .thenReturn("Onnistui!");

    Laskentakutsu laskentakutsu = createLaskentakutsu("successfulUuid");
    assertEquals(
        HakukohteenLaskennanTila.UUSI,
        createHttpClient("/valintalaskenta/laskekaikki").post(laskentakutsu, String.class));

    assertEquals(
        HakukohteenLaskennanTila.VALMIS,
        waitForEventualStatus(laskentakutsu, HakukohteenLaskennanTila.VALMIS));
  }

  @Test
  public void failingLaskentaIsRecognised() throws Exception {
    mockValisijoiteltavatJonotCall();
    when(getBean(ValintalaskentaService.class)
            .laskeKaikki(
                anyListOf(HakemusDTO.class),
                anyListOf(ValintaperusteetDTO.class),
                anyListOf(ValintaperusteetHakijaryhmaDTO.class),
                Matchers.eq(hakukohdeOid),
                any(String.class),
                anyBoolean()))
        .thenThrow(new RuntimeException(getClass().getSimpleName() + "-failure"));

    Laskentakutsu laskentakutsu = createLaskentakutsu("failingUuid");
    assertEquals(
        HakukohteenLaskennanTila.UUSI,
        createHttpClient("/valintalaskenta/laskekaikki").post(laskentakutsu, String.class));

    assertEquals(
        HakukohteenLaskennanTila.VIRHE,
        waitForEventualStatus(laskentakutsu, HakukohteenLaskennanTila.VIRHE));
  }

  @Test
  public void unfinishedLaskentaIsWaitedFor() throws Exception {
    mockValisijoiteltavatJonotCall();
    when(getBean(ValintalaskentaService.class)
            .laskeKaikki(
                anyListOf(HakemusDTO.class),
                anyListOf(ValintaperusteetDTO.class),
                anyListOf(ValintaperusteetHakijaryhmaDTO.class),
                Matchers.eq(hakukohdeOid),
                any(String.class),
                anyBoolean()))
        .thenAnswer(
            invocation -> {
              waitWhileMayFinishIsNotSet();
              return "Onnistui vähän myöhemmin!";
            });

    Laskentakutsu laskentakutsu = createLaskentakutsu("slowUuid");
    assertEquals(
        HakukohteenLaskennanTila.UUSI,
        createHttpClient("/valintalaskenta/laskekaikki").post(laskentakutsu, String.class));

    assertEquals(
        HakukohteenLaskennanTila.KESKEN,
        waitForEventualStatus(laskentakutsu, HakukohteenLaskennanTila.KESKEN));
    mayFinish = true;
    waitWhileNotFinished();
    assertEquals(
        HakukohteenLaskennanTila.VALMIS,
        waitForEventualStatus(laskentakutsu, HakukohteenLaskennanTila.VALMIS));
  }

  @Test
  public void crashingLaskenta() throws InterruptedException {
    when(getBean(ValisijoitteluKasittelija.class)
            .valisijoiteltavatJonot(anyListOf(LaskeDTO.class), any()))
        .thenThrow(
            new RuntimeException(
                ValisijoitteluKasittelija.class.getSimpleName()
                    + " call failing in "
                    + getClass().getSimpleName()));
    Laskentakutsu laskentakutsu = createLaskentakutsu("crashingUuid");
    assertEquals(
        HakukohteenLaskennanTila.UUSI,
        createHttpClient("/valintalaskenta/laskekaikki").post(laskentakutsu, String.class));
    assertEquals(
        HakukohteenLaskennanTila.VIRHE,
        waitForEventualStatus(laskentakutsu, HakukohteenLaskennanTila.VIRHE));
  }

  private void waitWhileMayFinishIsNotSet() throws InterruptedException {
    while (!mayFinish) {
      Thread.sleep(10);
    }
    finished = true;
  }

  private void waitWhileNotFinished() throws InterruptedException {
    while (!finished) {
      Thread.sleep(10);
    }
  }

  private void mockValisijoiteltavatJonotCall() {
    when(getBean(ValisijoitteluKasittelija.class)
            .valisijoiteltavatJonot(anyListOf(LaskeDTO.class), any()))
        .thenReturn(new ValisijoiteltavatJonot(Collections.emptySet(), Collections.emptyMap()));
  }

  private Laskentakutsu createLaskentakutsu(String uuid) {
    return new Laskentakutsu(
        new LaskeDTO(
            uuid + System.currentTimeMillis(),
            false,
            false,
            "hakukohdeOid",
            Collections.emptyList(),
            Collections.emptyList()),
        new SuoritustiedotDTO());
  }

  private String waitForEventualStatus(Laskentakutsu laskentakutsu, String expectedStatus)
      throws InterruptedException {
    int maxMillisToWait = 200;
    long startTime = System.currentTimeMillis();
    String statusFromServer;
    do {
      statusFromServer = fetchStatus(laskentakutsu);
      Thread.sleep(5);
    } while (!expectedStatus.equals(statusFromServer)
        && System.currentTimeMillis() < startTime + maxMillisToWait);
    return statusFromServer;
  }

  private String fetchStatus(Laskentakutsu laskentakutsu) {
    return createHttpClient("/valintalaskenta/status/" + laskentakutsu.getPollKey())
        .get(String.class);
  }

  private <T> T getBean(Class<T> requiredType) {
    return applicationContext.getBean(requiredType);
  }

  private WebClient createHttpClient(String path) {
    return new HttpResourceBuilder("valintalaskenta.valintalaskenta-laskenta-service")
        .address(ValintaLaskentaLaskentaJettyForTesting.rootUrl + path)
        .buildExposingWebClientDangerously()
        .getWebClient()
        .type(APPLICATION_JSON_TYPE);
  }
}
