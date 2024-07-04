package fi.vm.sade.valintalaskenta.tulos;

import static fi.vm.sade.valinta.sharedutils.http.HttpResource.CSRF_VALUE;
import static fi.vm.sade.valintalaskenta.tulos.RestClientUtil.*;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.javautils.nio.cas.CasClient;
import fi.vm.sade.javautils.nio.cas.CasClientBuilder;
import fi.vm.sade.javautils.nio.cas.CasConfig;
import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.javautils.opintopolku_spring_security.OidProvider;
import fi.vm.sade.javautils.opintopolku_spring_security.OrganisationHierarchyAuthorizer;
import fi.vm.sade.javautils.opintopolku_spring_security.ThreadLocalAuthorizer;
import fi.vm.sade.service.valintaperusteet.dto.HakukohdeImportDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResource;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.Response;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ValintalaskentaTulosConfiguration {
  public static final String CALLER_ID =
      "1.2.246.562.10.00000000001.valintalaskenta.valintalaskenta-tulos-service";

  @Bean(name = "oidProvider")
  public OidProvider oidProvider(
      @Value("${valintalaskenta-laskenta-service.organisaatio-service.url}")
          final String organisaatioServiceUrl,
      @Value("${root.organisaatio.oid}") final String rootOrganisaatioOid) {
    return new OidProvider(organisaatioServiceUrl, rootOrganisaatioOid, CALLER_ID);
  }

  @Bean(name = "organisationHierarchyAuthorizer")
  public OrganisationHierarchyAuthorizer organisationHierarchyAuthorizer() {
    return new OrganisationHierarchyAuthorizer();
  }

  @Bean(name = "authorizer")
  public Authorizer authorizer() {
    return new ThreadLocalAuthorizer();
  }

  @Bean(name = "modelMapper")
  public ValintalaskentaModelMapper modelMapper() {
    return new ValintalaskentaModelMapper();
  }

  @Value("${valintalaskenta-laskenta-service.global.http.connectionTimeoutMillis:59999}")
  private Integer clientConnectionTimeout;

  @Value("${valintalaskenta-laskenta-service.global.http.receiveTimeoutMillis:1799999}")
  private Integer clientReceiveTimeout;

  @Bean(name = "valintaperusteetCasClient")
  public CasClient valintaperusteetCasClient(
      @Value("${web.url.cas}") final String casUrl,
      @Value("${cas.service.valintaperusteet-service}") final String targetUrl,
      @Value("${valintalaskentakoostepalvelu.app.username.to.sijoittelu}") final String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.sijoittelu}") final String password) {
    return CasClientBuilder.build(
        new CasConfig.CasConfigBuilder(
                username,
                password,
                casUrl,
                targetUrl,
                CSRF_VALUE,
                CALLER_ID,
                "/j_spring_cas_security_check")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Bean(name = "valintaperusteetClient")
  public ValintaperusteetResource valintaperusteetResource(
      @Qualifier("valintaperusteetCasClient") final CasClient valintaperusteetCasClient,
      @Value("${valintalaskentakoostepalvelu.valintaperusteet.ilb.url}")
          final String valintaperusteetBaseUrl) {
    return new ValintaperusteetResource() {
      @Override
      public List<ValintatapajonoDTO> haeValintatapajonotSijoittelulle(final String hakukohdeOid) {
        final TypeToken<List<ValintatapajonoDTO>> typeToken = new TypeToken<>() {};
        return get(
            valintaperusteetCasClient,
            String.format(
                "%s/valintaperusteet/valintatapajono/%s", valintaperusteetBaseUrl, hakukohdeOid),
            typeToken,
            null,
            clientConnectionTimeout,
            clientReceiveTimeout);
      }

      @Override
      public Map<String, List<ValintatapajonoDTO>> haeValintatapajonotSijoittelulle(
          final List<String> oids) {
        final TypeToken<Map<String, List<ValintatapajonoDTO>>> typeToken = new TypeToken<>() {};
        return post(
            valintaperusteetCasClient,
            String.format("%s/valintaperusteet/valintatapajono", valintaperusteetBaseUrl),
            typeToken,
            oids,
            clientConnectionTimeout,
            clientReceiveTimeout);
      }

      @Override
      public List<ValintaperusteetDTO> haeValintaperusteet(
          final String hakukohdeOid, final Integer vaihe) {
        final TypeToken<List<ValintaperusteetDTO>> typeToken = new TypeToken<>() {};
        final Map<String, List<String>> queryParams =
            vaihe != null ? Map.of("vaihe", List.of(vaihe.toString())) : null;
        return get(
            valintaperusteetCasClient,
            String.format("%s/valintaperusteet/%s", valintaperusteetBaseUrl, hakukohdeOid),
            typeToken,
            queryParams,
            clientConnectionTimeout,
            clientReceiveTimeout);
      }

      @Override
      public List<ValintaperusteetHakijaryhmaDTO> haeHakijaryhmat(final String hakukohdeOid) {
        final TypeToken<List<ValintaperusteetHakijaryhmaDTO>> typeToken = new TypeToken<>() {};
        return get(
            valintaperusteetCasClient,
            String.format(
                "%s/valintaperusteet/hakijaryhma/%s", valintaperusteetBaseUrl, hakukohdeOid),
            typeToken,
            null,
            clientConnectionTimeout,
            clientReceiveTimeout);
      }

      @Override
      public Response tuoHakukohde(final HakukohdeImportDTO hakukohde) {
        final org.asynchttpclient.Response response =
            execute(
                valintaperusteetCasClient,
                request(
                        String.format("%s/valintaperusteet/tuoHakukohde", valintaperusteetBaseUrl),
                        "POST",
                        null)
                    .setBody(RestClientUtil.GSON.toJson(hakukohde))
                    .addHeader("Content-Type", "application/json")
                    .setRequestTimeout(clientConnectionTimeout)
                    .setReadTimeout(clientReceiveTimeout)
                    .build());
        return Response.status(response.getStatusCode()).entity(response.getResponseBody()).build();
      }

      @Override
      public Boolean readAutomaattinenSijoitteluunSiirto(final String oid) {
        final TypeToken<Boolean> typeToken = new TypeToken<>() {};
        return get(
            valintaperusteetCasClient,
            String.format(
                "%s/valintaperusteet/%s/automaattinenSiirto", valintaperusteetBaseUrl, oid),
            typeToken,
            null,
            clientConnectionTimeout,
            clientReceiveTimeout);
      }

      @Override
      public ValintatapajonoDTO updateAutomaattinenSijoitteluunSiirto(
          final String oid, final Boolean value, final HttpServletRequest httpServletRequest) {
        final TypeToken<ValintatapajonoDTO> typeToken = new TypeToken<>() {};
        return post(
            valintaperusteetCasClient,
            String.format(
                "%s/valintaperusteet/%s/automaattinenSiirto", valintaperusteetBaseUrl, oid),
            typeToken,
            value,
            clientConnectionTimeout,
            clientReceiveTimeout);
      }
    };
  }
}
