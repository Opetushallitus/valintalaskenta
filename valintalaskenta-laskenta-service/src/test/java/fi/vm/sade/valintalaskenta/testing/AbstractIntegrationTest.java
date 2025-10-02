package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.valintalaskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.*;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosHarkinnanvarainenHyvaksyminenRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosJarjestyskriteerihistoriaRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosMuokattuJonosijaRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosValintatapajonoRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.ValintapisteRepository;
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
@Import({TestConfigurationWithMocks.class, KoosteTestProfileConfiguration.class})
@ActiveProfiles("test")
public abstract class AbstractIntegrationTest {

  @Autowired protected HakijaryhmaRepository hakijaryhmaRepository;

  @Autowired protected ValinnanvaiheRepository valinnanvaiheRepository;

  @Autowired protected ValintakoeOsallistuminenRepository valintakoeOsallistuminenRepository;

  @Autowired protected TulosValintatapajonoRepository tulosValintatapajonoRepository;

  @Autowired protected JonosijaRepository jonosijaRepository;

  @Autowired protected TulosMuokattuJonosijaRepository tulosMuokattuJonosijaRepository;

  @Autowired protected ValintapisteRepository valintapisteRepository;

  @Autowired
  protected TulosHarkinnanvarainenHyvaksyminenRepository
      tulosHarkinnanvarainenHyvaksyminenRepository;

  @Autowired
  protected TulosJarjestyskriteerihistoriaRepository tulosJarjestyskriteerihistoriaRepository;

  @LocalServerPort protected Integer port;

  @BeforeEach
  public void setUp() {
    tulosHarkinnanvarainenHyvaksyminenRepository.deleteAll();
    tulosMuokattuJonosijaRepository.deleteAll();
    jonosijaRepository.deleteAll();
    tulosValintatapajonoRepository.deleteAll();
    valinnanvaiheRepository.deleteAll();
    hakijaryhmaRepository.deleteAll();
    valintakoeOsallistuminenRepository.deleteAll();
    tulosJarjestyskriteerihistoriaRepository.deleteAll();
    valintapisteRepository.truncateWithHistory();
  }
}
