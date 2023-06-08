package fi.vm.sade.valintalaskenta.laskenta.config.properties;

import fi.vm.sade.properties.OphProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
public class UrlConfiguration extends OphProperties {
  public UrlConfiguration() {
    addOptionalFiles(
        "classpath:valintalaskenta-laskenta-service-oph.properties",
        "file:///${user.home:''}/oph-configuration/common.properties",
        "file:///${user.home:''}/oph-configuration/valinta.properties",
        "file:///${user.home:''}/oph-configuration/valintalaskenta-laskenta-service.properties",
        "file:///${user.home:''}/oph-configuration/override.properties");
  }
}
