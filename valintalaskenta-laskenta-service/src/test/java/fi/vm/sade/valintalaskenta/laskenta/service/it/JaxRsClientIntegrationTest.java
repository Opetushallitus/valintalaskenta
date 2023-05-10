package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertThat;

import fi.vm.sade.valintalaskenta.laskenta.testing.ValintaLaskentaLaskentaAppForTesting;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Controller;

public class JaxRsClientIntegrationTest {
  private ApplicationContext applicationContext;

  @Before
  public void setUp() {
    ValintaLaskentaLaskentaAppForTesting.main(new String[] {});
    applicationContext =
        ValintaLaskentaLaskentaAppForTesting.ApplicationContextGetter.applicationContext;
  }

  @Test
  public void testJaxrsClient() {
    ResourceForTesting jaxrsClient = applicationContext.getBean(ResourceForTesting.class);
    assertThat(jaxrsClient.root(), containsString("valintalaskenta-laskenta-service"));
  }

  @Controller
  @Path("/")
  public interface ResourceForTesting {

    @GET
    @Path("../")
    @Produces("text/plain")
    String root();
  }
}
