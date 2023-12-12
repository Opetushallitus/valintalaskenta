package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.*;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.HarkinnanvarainenHyvaksyminenRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.JarjestyskriteerihistoriaRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.MuokattuJonosijaRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.ValintatapajonoRepository;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.*;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest(
    webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
    classes = {App.class},
    args = {"--add-opens=java.base/java.lang=ALL-UNNAMED"})
@Import(TestConfigurationWithMocks.class)
@ActiveProfiles("test")
public abstract class AbstractIntegrationTest {

  @Autowired protected HakijaryhmaRepository hakijaryhmaRepository;

  @Autowired protected ValinnanvaiheRepository valinnanvaiheRepository;

  @Autowired protected ValintakoeOsallistuminenRepository valintakoeOsallistuminenRepository;

  @Autowired protected ValintatapajonoRepository valintatapajonoRepository;

  @Autowired protected JonosijaRepository jonosijaRepository;

  @Autowired protected MuokattuJonosijaRepository muokattuJonosijaRepository;

  @Autowired
  protected HarkinnanvarainenHyvaksyminenRepository harkinnanvarainenHyvaksyminenRepository;

  @Autowired protected JarjestyskriteerihistoriaRepository jarjestyskriteerihistoriaRepository;

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
    jarjestyskriteerihistoriaRepository.deleteAll();
  }
}
