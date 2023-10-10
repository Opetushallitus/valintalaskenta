package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.valintalaskenta.laskenta.dao.repository.HakijaryhmaRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValinnanvaiheRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValintakoeOsallistuminenRepository;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Profile;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
  args = {"--add-opens=java.base/java.lang=ALL-UNNAMED"})
@RunWith(SpringJUnit4ClassRunner.class)
@ActiveProfiles("test-mockless")
public class AbstractMocklessIntegrationTest {

  @Autowired
  protected HakijaryhmaRepository hakijaryhmaRepository;

  @Autowired
  protected ValinnanvaiheRepository valinnanvaiheRepository;

  @Autowired
  protected ValintakoeOsallistuminenRepository valintakoeOsallistuminenRepository;

  @LocalServerPort
  protected Integer port;

  @Before
  public void setUp() {
    valinnanvaiheRepository.deleteAll();
    hakijaryhmaRepository.deleteAll();
    valintakoeOsallistuminenRepository.deleteAll();
  }

  @Profile("test-mockless")
  @TestConfiguration
  static class TestConfig {
    @Bean
    public ApplicationContextGetter applicationContextGetter() {
      return new ApplicationContextGetter();
    }
  }

  public static class ApplicationContextGetter implements ApplicationContextAware {
    public static ApplicationContext applicationContext;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
      TestApp.ApplicationContextGetter.applicationContext = applicationContext;
    }
  }
}
