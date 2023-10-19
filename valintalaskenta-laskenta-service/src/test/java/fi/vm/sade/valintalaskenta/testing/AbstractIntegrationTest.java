package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.*;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.HarkinnanvarainenHyvaksyminenRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.MuokattuJonosijaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.*;
import org.springframework.test.context.ActiveProfiles;

// @EnableJdbcRepositories(basePackages = "fi.vm.sade.valintalaskenta.laskenta.dao.repository")
@SpringBootTest(
    webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
    classes = {App.class},
    args = {"--add-opens=java.base/java.lang=ALL-UNNAMED"})
@Import(TestConfigurationWithMocks.class)
@ActiveProfiles("test")
// @ContextConfiguration(locations = "classpath:application-context-test.xml")
// @EnableAutoConfiguration(exclude=AbstractMocklessIntegrationTest.class)
// @ExtendWith(SpringBExtension.class)
/*@TestExecutionListeners(
listeners = {
        DirtiesContextTestExecutionListener.class
})*/
// @EnableWebSecurity(debug = true)
// @EnableJpaRepositories(basePackages = "fi.vm.sade.valintalaskenta.laskenta.dao.repository")
// @EntityScan(basePackages = "fi.vm.sade.valintalaskenta.domain.*")
public abstract class AbstractIntegrationTest {

  @Autowired protected HakijaryhmaRepository hakijaryhmaRepository;

  @Autowired protected ValinnanvaiheRepository valinnanvaiheRepository;

  @Autowired protected ValintakoeOsallistuminenRepository valintakoeOsallistuminenRepository;

  @Autowired protected ValintatapajonoRepository valintatapajonoRepository;

  @Autowired protected JonosijaRepository jonosijaRepository;

  @Autowired protected MuokattuJonosijaRepository muokattuJonosijaRepository;

  @Autowired
  protected HarkinnanvarainenHyvaksyminenRepository harkinnanvarainenHyvaksyminenRepository;

  @LocalServerPort protected Integer port;

  @BeforeEach
  public void setUp() {
    harkinnanvarainenHyvaksyminenRepository.deleteAll();
    muokattuJonosijaRepository.deleteAll();
    jonosijaRepository.deleteAll();
    valintatapajonoRepository.deleteAll();
    valinnanvaiheRepository.deleteAll();
    hakijaryhmaRepository.deleteAll();
    valintakoeOsallistuminenRepository.deleteAll();
  }
}
