package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import java.io.IOException;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;
import javax.servlet.*;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.Dsl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class KoosteTestProfileConfiguration {
  private static final AsyncHttpClient asyncHttpClient = Dsl.asyncHttpClient();
  private static RestCasClient REST_CAS_CLIENT =
      new RestCasClient(
          request -> asyncHttpClient.executeRequest(request).toCompletableFuture()) {};

  @Bean(name = "AtaruCasClient")
  public RestCasClient getAtaruCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "HakuAppCasClient")
  public RestCasClient getHakuAppCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "SijoitteluServiceCasClient")
  public RestCasClient getSijoitteluServiceCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "HakukohderyhmapalveluCasClient")
  public RestCasClient getHakukohderyhmapalveluCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "KoutaCasClient")
  public RestCasClient getKoutaCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ViestintapalveluCasClient")
  public RestCasClient getViestintapalveluCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "OppijanumerorekisteriCasClient")
  public RestCasClient getOppijanumerorekisteriCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ValintapisteServiceCasClient")
  public RestCasClient getValintapisteServiceCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "SuoritusrekisteriCasClient")
  public RestCasClient getSuoritusrekisteriCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ValintalaskentaCasClient")
  public RestCasClient getValintalaskentaCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ValintaperusteetCasClient")
  public RestCasClient getValintaperusteetCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ryhmasahkopostiCasClient")
  public RestCasClient getRyhmasahkopostiCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "OppijantunnistusCasClient")
  public RestCasClient getOppijantunnistusCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "SeurantaCasClient")
  public RestCasClient getSeurantaCasClient() {
    return REST_CAS_CLIENT;
  }
}
