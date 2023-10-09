package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.HakijaryhmaRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValinnanvaiheRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValintakoeOsallistuminenRepository;
import fi.vm.sade.valintalaskenta.laskenta.testing.DefaultTestConfiguration;
import org.junit.Before;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.*;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;
import org.testcontainers.containers.PostgreSQLContainer;


//@EnableJdbcRepositories(basePackages = "fi.vm.sade.valintalaskenta.laskenta.dao.repository")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
  args = {"--add-opens=java.base/java.lang=ALL-UNNAMED"})
//@Import(DefaultTestConfiguration.class)
//@ActiveProfiles("test")
//@ContextConfiguration(locations = "classpath:application-context-test.xml")
/*@ComponentScan(basePackages = {"fi.vm.sade.valintalaskenta.laskenta.*"},
  excludeFilters = {
    @ComponentScan.Filter(type = FilterType.ASPECTJ, pattern = "fi.vm.sade.valintalaskenta.laskenta.config.*"),
    @ComponentScan.Filter(type = FilterType.ASPECTJ, pattern = "fi.vm.sade.valintalaskenta.tulos.*"),
    @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = App.class)
  })*/
@RunWith(SpringJUnit4ClassRunner.class)
/*@TestExecutionListeners(
        listeners = {
                DirtiesContextTestExecutionListener.class
        })*/
//@EnableWebSecurity(debug = true)
//@EnableJpaRepositories(basePackages = "fi.vm.sade.valintalaskenta.laskenta.dao.repository")
//@EntityScan(basePackages = "fi.vm.sade.valintalaskenta.domain.*")
public abstract class AbstractIntegrationTest {

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

}
