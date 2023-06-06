package fi.vm.sade.valintalaskenta.laskenta.config.properties;

import fi.vm.sade.properties.OphProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

@Configuration
public class UrlConfiguration extends OphProperties {
  @Value("${cas.service.valintalaskenta-service}")
  private String casService;

  @Value("${cas.callback.valintalaskenta-service}")
  private String casCallback;

  @Autowired
  public UrlConfiguration(final Environment environment) {
    addOptionalFiles(
        "classpath:valintalaskenta-laskenta-service-oph.properties",
        "file:///${user.home:''}/oph-configuration/common.properties",
        "file:///${user.home:''}/oph-configuration/valinta.properties",
        "file:///${user.home:''}/oph-configuration/valintalaskenta-laskenta-service.properties",
        "file:///${user.home:''}/oph-configuration/override.properties");
    addOverride("host-cas", environment.getRequiredProperty("host.host-cas"));
    addOverride("host-virkailija", environment.getRequiredProperty("host.host-virkailija"));
    addOverride("cas_key", "valintalaskenta-service");
    addOverride("cas_service", casService);
    addOverride("cas_callback_url", casCallback);
    addOverride("spring_security_default_access", "isAuthenticated()");
    addOverride("valintalaskenta-laskenta-service.parallelism", "-1");
  }
}
