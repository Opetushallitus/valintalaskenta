package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.asynchttpclient.Dsl.asyncHttpClient;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.PistetietoWrapper;
import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.AtaruHakemus;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.AtaruResource;
import fi.vm.sade.valintalaskenta.laskenta.service.valintapiste.ValintapisteService;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintapisteDAO;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.stream.IntStream;
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.Request;
import org.asynchttpclient.RequestBuilder;
import org.asynchttpclient.Response;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

public class ValintapisteResourceIntegrationTest extends AbstractIntegrationTest {
  @Autowired private ValintapisteDAO valintapisteDAO;
  @Autowired private AtaruResource ataruResource;
  @Autowired private ObjectMapper objectMapper;

  private final AsyncHttpClient asyncHttpClient = asyncHttpClient();

  private static final String HAKEMUS_OID = "hakemus-oid";
  private static final String OPPIJA_OID = "oppija-oid";
  private static final Valintapiste DEFAULT_PISTE =
      new Valintapiste(
          HAKEMUS_OID, "Tunniste", "42.0", Osallistumistieto.OSALLISTUI, "tallettaja-oid");
  private static final Valintapiste OTHER_PISTE =
      new Valintapiste(
          HAKEMUS_OID, "Tunniste2", null, Osallistumistieto.EI_OSALLISTUNUT, "toinen-tallettaja");

  @Nested
  public class FindValintapisteetForHakukohde {
    private final String hakuOid = "1.2.246.562.29.00000000000000000001";
    private final String hakukohdeOid = "1.2.246.562.20.00000000000000000001";

    private final String uri =
        String.format("valintapisteet/haku/%s/hakukohde/%s", hakuOid, hakukohdeOid);

    private final AtaruHakemus ataruHakemus = new AtaruHakemus(HAKEMUS_OID, OPPIJA_OID);

    @BeforeEach
    void resetMocks() {
      Mockito.reset(ataruResource);
    }

    @Test
    public void returnsEmptyArrayWhenAtaruHasNoHakemukset() {
      Mockito.when(ataruResource.getHakemukset(hakuOid, hakukohdeOid)).thenReturn(List.of());

      Response response = get(uri);

      assertThat(response.getStatusCode()).isEqualTo(200);
      assertThat(response.getResponseBody()).isEqualTo("[]");
      String lastModified = response.getHeader("Last-Modified");
      assertThat(lastModified).isNull();
    }

    @Test
    public void returnsHakemusAndOppijaOidsWithoutPisteetWhenThereAreNoPisteet()
        throws JsonProcessingException {
      Mockito.when(ataruResource.getHakemukset(hakuOid, hakukohdeOid))
          .thenReturn(List.of(ataruHakemus));

      Response response = get(uri);

      assertThat(response.getStatusCode()).isEqualTo(200);
      List<PistetietoWrapper> body =
          objectMapper.readValue(response.getResponseBody(), new TypeReference<>() {});
      assertThat(body).containsExactly(ataruHakemus.toPistetietoWrapper());
    }

    @Test
    public void returnsPistetiedotWhenWeHaveThem() throws JsonProcessingException {
      valintapisteDAO.upsertValintapiste(DEFAULT_PISTE);
      valintapisteDAO.upsertValintapiste(OTHER_PISTE);
      Mockito.when(ataruResource.getHakemukset(hakuOid, hakukohdeOid))
          .thenReturn(List.of(ataruHakemus));

      Response response = get(uri);

      assertThat(response.getStatusCode()).isEqualTo(200);
      List<PistetietoWrapper> body =
          objectMapper.readValue(response.getResponseBody(), new TypeReference<>() {});
      assertThat(body)
          .containsExactly(
              ataruHakemus.toPistetietoWrapper(
                  DEFAULT_PISTE.toPistetieto(), OTHER_PISTE.toPistetieto()));
    }
  }

  @Nested
  public class FindValintapisteetForHakemus {
    String uri = String.format("valintapisteet/hakemus/%s/oppija/%s", HAKEMUS_OID, OPPIJA_OID);

    @Test
    public void returnsWrapperWithoutPisteetWhenHakemusOidNotFound()
        throws JsonProcessingException {
      Response response = get(uri);

      assertThat(response.getStatusCode()).isEqualTo(200);
      PistetietoWrapper body =
          objectMapper.readValue(response.getResponseBody(), PistetietoWrapper.class);
      assertThat(body.hakemusOID()).isEqualTo(HAKEMUS_OID);
      assertThat(body.oppijaOID()).isEqualTo(OPPIJA_OID);
      assertThat(body.pisteet()).isEmpty();
    }

    @Test
    public void returnsPisteetWhenHakemusOidFound() throws JsonProcessingException {
      valintapisteDAO.upsertValintapiste(DEFAULT_PISTE);
      valintapisteDAO.upsertValintapiste(OTHER_PISTE);

      Response response = get(uri);

      assertThat(response.getStatusCode()).isEqualTo(200);
      PistetietoWrapper body =
          objectMapper.readValue(response.getResponseBody(), PistetietoWrapper.class);
      assertThat(body.hakemusOID()).isEqualTo(HAKEMUS_OID);
      assertThat(body.oppijaOID()).isEqualTo(OPPIJA_OID);
      assertThat(body.pisteet())
          .containsExactlyInAnyOrder(DEFAULT_PISTE.toPistetieto(), OTHER_PISTE.toPistetieto());
    }

    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {"oppija", "1.1.1.1"})
    public void echoesOppijaOidBlindly(String oppijaOid) throws JsonProcessingException {
      String uri = String.format("valintapisteet/hakemus/%s/oppija/%s", HAKEMUS_OID, oppijaOid);

      Response response = get(uri);

      assertThat(response.getStatusCode()).isEqualTo(200);
      PistetietoWrapper body =
          objectMapper.readValue(response.getResponseBody(), PistetietoWrapper.class);
      assertThat(body.oppijaOID()).isEqualTo(String.valueOf(oppijaOid));
    }
  }

  @Nested
  public class FindValintapisteetWithHakemusoids {
    private final String uri = "valintapisteet/pisteet-with-hakemusoids";

    @Test
    public void returnsEmptyListWhenHakemusOidsIsEmpty() {
      Response response = post(uri, List.of());

      assertThat(response.getStatusCode()).isEqualTo(200);
      String lastModified = response.getHeader("Last-Modified");
      assertThat(lastModified).isNull();
      assertThat(response.getResponseBody()).isEqualTo("[]");
    }

    @Test
    public void returnsLastModifiedInHeader() {
      valintapisteDAO.upsertValintapiste(DEFAULT_PISTE);

      Response response = post(uri, List.of(HAKEMUS_OID, "Non-existing"));

      assertThat(response.getStatusCode()).isEqualTo(200);
      String lastModified = response.getHeader("Last-Modified");
      assertThat(lastModified).isNotBlank();
      assertThat(ZonedDateTime.parse(lastModified))
          .isCloseTo(ZonedDateTime.now(), within(1, ChronoUnit.SECONDS));
    }

    @Test
    public void returnsBothMatchingAndNotMatching() throws JsonProcessingException {
      valintapisteDAO.upsertValintapiste(DEFAULT_PISTE);
      valintapisteDAO.upsertValintapiste(OTHER_PISTE);

      Response response = post(uri, List.of(HAKEMUS_OID, "Non-existing"));

      assertThat(response.getStatusCode()).isEqualTo(200);
      List<PistetietoWrapper> body =
          objectMapper.readValue(response.getResponseBody(), new TypeReference<>() {});
      assertThat(body)
          .containsExactly(
              toWrapperList(DEFAULT_PISTE, OTHER_PISTE).get(0),
              new PistetietoWrapper("Non-existing", List.of()));
    }

    @Test
    public void returnsErrorWhenTooManyOids() {
      List<String> oids = IntStream.range(0, 32768).mapToObj(i -> "hakemus-" + i).toList();

      Response response = post(uri, oids);

      assertThat(response.getStatusCode()).isEqualTo(400);
    }
  }

  @Nested
  public class UpdatePistetiedot {
    private final Valintapiste existingPiste =
        new Valintapiste(
            "UPDATE_TEST",
            "TRY_TO_UPDATE",
            "UPDATE_FAILED!",
            Osallistumistieto.OSALLISTUI,
            "1.2.3.4");

    private final Valintapiste new1 =
        new Valintapiste("1", "A", "A", Osallistumistieto.OSALLISTUI, "1.2.3.4");
    private final Valintapiste new2 =
        new Valintapiste("2", "B", "B", Osallistumistieto.OSALLISTUI, "1.2.3.4");

    private void insertTestData() {
      valintapisteDAO.upsertValintapiste(existingPiste);
    }

    @Nested
    public class WithoutSavePartially {

      private final String uri = "valintapisteet/pisteet-with-hakemusoids";

      @Test
      public void upsertsValintapisteet() {
        insertTestData();
        ZonedDateTime savedAt =
            valintapisteDAO.lastModifiedForHakemukset(List.of("UPDATE_TEST")).orElseThrow();
        Map<String, String> headers =
            Map.of("If-Unmodified-Since", savedAt.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
        List<PistetietoWrapper> requestBody =
            toWrapperList(existingPiste.withArvo("UPDATE_SUCCEEDED!"), new1, new2);

        Response response = put(uri, headers, requestBody);

        assertThat(response.getStatusCode()).isEqualTo(200);
        assertThat(response.getResponseBody()).isEqualTo("");

        List<Valintapiste> saved =
            valintapisteDAO.findValintapisteetForHakemukset(List.of("UPDATE_TEST", "1", "2"));
        assertThat(saved).containsExactly(existingPiste.withArvo("UPDATE_SUCCEEDED!"), new1, new2);
      }

      @Test
      public void failsWhenEvenOneTooLate() {
        insertTestData();
        Map<String, String> headers =
            Map.of("If-Unmodified-Since", "2017-10-04T14:36:01.059+03:00");
        List<PistetietoWrapper> requestBody =
            toWrapperList(existingPiste.withArvo("UPDATE_SUCCEEDED!"), new1);

        Response response = put(uri, headers, requestBody);

        assertThat(response.getStatusCode()).isEqualTo(409);
        assertThat(response.getResponseBody()).isEqualTo("[\"UPDATE_TEST\"]");

        List<Valintapiste> saved = valintapisteDAO.findByHakemusId("UPDATE_TEST");
        assertThat(saved).singleElement().isEqualTo(existingPiste);
      }
    }

    @Nested
    public class WithSavePartially {
      private static final String URI =
          "valintapisteet/pisteet-with-hakemusoids?save-partially=true";

      @Test
      public void savesNew() {
        insertTestData();
        Map<String, String> headers =
            Map.of("If-Unmodified-Since", "2017-10-04T14:36:01.059+03:00");
        List<PistetietoWrapper> requestBody = toWrapperList(new1, new1.withTunniste("OTHER"));

        Response response = put(URI, headers, requestBody);

        assertThat(response.getStatusCode()).isEqualTo(200);
        assertThat(response.getResponseBody()).isEqualTo("[]");

        List<Valintapiste> saved = valintapisteDAO.findByHakemusId(new1.hakemusOid());
        assertThat(saved).containsExactly(new1, new1.withTunniste("OTHER"));
      }

      @Test
      public void savesNewWithMissingArvo() {
        Map<String, String> headers =
            Map.of("If-Unmodified-Since", "2017-10-04T14:36:01.059+03:00");
        String requestBody =
            """
               [{
                 "hakemusOID": "UPDATE_TEST",
                 "pisteet": [
                   {
                     "tunniste": "TRY_TO_UPDATE",
                     "osallistuminen": "OSALLISTUI",
                     "tallettaja": "1.2.3.4"
                   }
                 ]
               }]""";

        Response response = put(URI, headers, requestBody);

        assertThat(response.getStatusCode()).isEqualTo(200);
        assertThat(response.getResponseBody()).isEqualTo("[]");

        List<Valintapiste> saved = valintapisteDAO.findByHakemusId("UPDATE_TEST");
        assertThat(saved).singleElement().isEqualTo(existingPiste.withArvo(null));
      }

      @Test
      public void succeedsWithoutSavingWhenTooLate() {
        insertTestData();
        Map<String, String> headers =
            Map.of("If-Unmodified-Since", "2017-10-04T14:36:01.059+03:00");
        List<PistetietoWrapper> requestBody =
            toWrapperList(
                existingPiste.withArvo("UPDATE_SUCCEEDED"), existingPiste.withTunniste("OTHER"));

        Response response = put(URI, headers, requestBody);

        assertThat(response.getStatusCode()).isEqualTo(200);
        assertThat(response.getResponseBody()).isEqualTo("[\"UPDATE_TEST\"]");

        List<Valintapiste> saved = valintapisteDAO.findByHakemusId("UPDATE_TEST");
        assertThat(saved).singleElement().isEqualTo(existingPiste);
      }

      @Test
      public void savesPartialResults() {
        insertTestData();
        Map<String, String> headers =
            Map.of("If-Unmodified-Since", "2017-10-04T14:36:01.059+03:00");
        List<PistetietoWrapper> requestBody =
            toWrapperList(
                existingPiste.withArvo("UPDATE_SUCCEEDED"),
                existingPiste.withTunniste("OTHER"),
                new1);

        Response response = put(URI, headers, requestBody);

        assertThat(response.getStatusCode()).isEqualTo(200);
        assertThat(response.getResponseBody()).isEqualTo("[\"UPDATE_TEST\"]");

        List<Valintapiste> saved =
            valintapisteDAO.findValintapisteetForHakemukset(List.of("UPDATE_TEST", "1"));
        assertThat(saved).containsExactly(existingPiste, new1);
      }
    }
  }

  private Response get(String uri) {
    return call(uri, "GET", Map.of(), null);
  }

  private Response post(String uri, Object body) {
    return call(uri, "POST", Map.of(), body);
  }

  private Response put(String uri, Map<String, String> extraHeaders, Object body) {
    return call(uri, "PUT", extraHeaders, body);
  }

  private Response call(String uri, String method, Map<String, String> extraHeaders, Object body) {
    final String address = "http://localhost:" + port + "/resources/" + uri;
    try {
      return asyncHttpClient
          .executeRequest(request(address, method, extraHeaders, body))
          .toCompletableFuture()
          .get();
    } catch (InterruptedException | ExecutionException e) {
      throw new RuntimeException(e);
    }
  }

  private Request request(
      String url, String method, Map<String, String> extraHeaders, Object body) {
    RequestBuilder requestBuilder = new RequestBuilder().setUrl(url).setMethod(method);
    if (extraHeaders != null) {
      requestBuilder.setSingleHeaders(extraHeaders);
    }
    if (body != null) {
      if (body instanceof String) {
        requestBuilder.setBody((String) body);
      } else {
        requestBuilder.setBody(asJson(body));
      }
      requestBuilder.setHeader("Content-Type", MediaType.APPLICATION_JSON_VALUE);
    }
    return requestBuilder.setHeader("Accept", MediaType.APPLICATION_JSON_VALUE).build();
  }

  private List<PistetietoWrapper> toWrapperList(Valintapiste... valintapisteet) {
    return ValintapisteService.convertToWrappers(
            ValintapisteService.groupByHakemus(Arrays.stream(valintapisteet).toList()))
        .toList();
  }

  private String asJson(Object o) {
    try {
      return objectMapper.writeValueAsString(o);
    } catch (JsonProcessingException e) {
      throw new RuntimeException(e);
    }
  }
}
