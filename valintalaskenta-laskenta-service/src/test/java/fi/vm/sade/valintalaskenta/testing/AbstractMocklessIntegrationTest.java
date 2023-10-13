package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.*;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.HarkinnanvarainenHyvaksyminenRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.MuokattuJonosijaRepository;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
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
  classes = {App.class},
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

  @Autowired
  protected MuokattuJonosijaRepository muokattuJonosijaRepository;

  @Autowired
  protected HarkinnanvarainenHyvaksyminenRepository harkinnanvarainenHyvaksyminenRepository;

  @Autowired
  protected JonosijaRepository jonosijaRepository;

  @Autowired
  protected ValintatapajonoRepository valintatapajonoRepository;

  @LocalServerPort
  protected Integer port;

  @Before
  public void setUp() {
    harkinnanvarainenHyvaksyminenRepository.deleteAll();
    muokattuJonosijaRepository.deleteAll();
    jonosijaRepository.deleteAll();
    valintatapajonoRepository.deleteAll();
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
