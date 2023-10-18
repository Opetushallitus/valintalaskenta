package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static org.asynchttpclient.Dsl.asyncHttpClient;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

import fi.vm.sade.valintalaskenta.domain.HakukohteenLaskennanTila;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.Laskentakutsu;
import fi.vm.sade.valintalaskenta.domain.dto.SuoritustiedotDTO;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija.ValisijoiteltavatJonot;
import fi.vm.sade.valintalaskenta.testing.AbstractMocklessIntegrationTest;
import fi.vm.sade.valintalaskenta.tulos.RestClientUtil;
import java.util.Collections;
import java.util.concurrent.ExecutionException;
import org.asynchttpclient.*;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

public class ValintalaskentaResourceHttpIntegrationTest extends AbstractMocklessIntegrationTest {
  private final String hakukohdeOid = "hakukohdeOid";

  @Autowired private ApplicationContext applicationContext;
  private boolean mayFinish;
  private boolean finished;
  private AsyncHttpClient asyncHttpClient = asyncHttpClient();

  @Test
  public void successfulLaskentaBecomesReady() throws Exception {
    mockValisijoiteltavatJonotCall();
    when(getBean(ValintalaskentaService.class)
            .laskeKaikki(
                anyList(),
                anyList(),
                anyList(),
                ArgumentMatchers.eq(hakukohdeOid),
                any(String.class),
                anyBoolean()))
        .thenReturn("Onnistui!");

    Laskentakutsu laskentakutsu = createLaskentakutsu("successfulUuid");
    assertEquals(
        HakukohteenLaskennanTila.UUSI, call("valintalaskenta/laskekaikki", "POST", laskentakutsu));

    assertEquals(
        HakukohteenLaskennanTila.VALMIS,
        waitForEventualStatus(laskentakutsu, HakukohteenLaskennanTila.VALMIS));
  }

  @Test
  public void failingLaskentaIsRecognised() throws Exception {
    mockValisijoiteltavatJonotCall();
    when(getBean(ValintalaskentaService.class)
            .laskeKaikki(
                anyList(),
                anyList(),
                anyList(),
                ArgumentMatchers.eq(hakukohdeOid),
                any(String.class),
                anyBoolean()))
        .thenThrow(new RuntimeException(getClass().getSimpleName() + "-failure"));

    Laskentakutsu laskentakutsu = createLaskentakutsu("failingUuid");
    assertEquals(
        HakukohteenLaskennanTila.UUSI, call("valintalaskenta/laskekaikki", "POST", laskentakutsu));

    assertEquals(
        HakukohteenLaskennanTila.VIRHE,
        waitForEventualStatus(laskentakutsu, HakukohteenLaskennanTila.VIRHE));
  }

  @Test
  public void unfinishedLaskentaIsWaitedFor() throws Exception {
    mockValisijoiteltavatJonotCall();
    when(getBean(ValintalaskentaService.class)
            .laskeKaikki(
                anyList(),
                anyList(),
                anyList(),
                ArgumentMatchers.eq(hakukohdeOid),
                any(String.class),
                anyBoolean()))
        .thenAnswer(
            invocation -> {
              waitWhileMayFinishIsNotSet();
              return "Onnistui vähän myöhemmin!";
            });

    Laskentakutsu laskentakutsu = createLaskentakutsu("slowUuid");
    assertEquals(
        HakukohteenLaskennanTila.UUSI, call("valintalaskenta/laskekaikki", "POST", laskentakutsu));

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
  public void crashingLaskenta() throws InterruptedException, ExecutionException {
    when(getBean(ValisijoitteluKasittelija.class).valisijoiteltavatJonot(anyList(), any()))
        .thenThrow(
            new RuntimeException(
                ValisijoitteluKasittelija.class.getSimpleName()
                    + " call failing in "
                    + getClass().getSimpleName()));
    Laskentakutsu laskentakutsu = createLaskentakutsu("crashingUuid");
    assertEquals(
        HakukohteenLaskennanTila.UUSI, call("valintalaskenta/laskekaikki", "POST", laskentakutsu));
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
    when(getBean(ValisijoitteluKasittelija.class).valisijoiteltavatJonot(anyList(), any()))
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
      throws InterruptedException, ExecutionException {
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

  private String fetchStatus(Laskentakutsu laskentakutsu)
      throws ExecutionException, InterruptedException {
    return call(
        String.format("valintalaskenta/status/%s", laskentakutsu.getPollKey()), "GET", null);
  }

  private <T> T getBean(Class<T> requiredType) {
    return applicationContext.getBean(requiredType);
  }

  private Request request(final String url, final String method, final Object body) {
    final RequestBuilder requestBuilder =
        new RequestBuilder().setUrl(url).setMethod(method).setHeader("Accept", "text/plain");
    if (body != null) {
      requestBuilder.setBody(RestClientUtil.GSON.toJson(body));
      requestBuilder.setHeader("Content-Type", "application/json");
    }
    return requestBuilder.build();
  }

  private String call(final String uri, final String method, final Object body)
      throws ExecutionException, InterruptedException {
    final String baseAddress = "http://localhost:" + port + "/resources";
    return asyncHttpClient
        .executeRequest(request(String.format("%s/%s", baseAddress, uri), method, body))
        .toCompletableFuture()
        .get()
        .getResponseBody();
  }
}
