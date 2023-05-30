package fi.vm.sade.valintalaskenta.laskenta.service.it;

import static org.junit.Assert.assertEquals;

import fi.vm.sade.valintalaskenta.laskenta.testing.TestApp;
import java.util.Map;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Controller;

public class JaxRsClientIntegrationTest {
  private ApplicationContext applicationContext;

  @Before
  public void setUp() {
    TestApp.startTestApp();
    applicationContext = TestApp.ApplicationContextGetter.applicationContext;
  }

  @After
  public void tearDown() {
    TestApp.stopTestApp();
  }

  @Test
  public void testJaxrsClient() {
    ResourceForTesting jaxrsClient = applicationContext.getBean(ResourceForTesting.class);
    assertEquals(jaxrsClient.root(), Map.of("status", "UP"));
  }

  @Controller
  @Path("/")
  public interface ResourceForTesting {

    @GET
    @Path("../actuator/health")
    @Produces("application/json")
    Map<String, String> root();
  }
}
