package fi.vm.sade.valintalaskenta.laskenta.config.properties;

import fi.vm.sade.properties.OphProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
public class UrlConfiguration extends OphProperties {
  public UrlConfiguration() {
    addOptionalFiles("file:///home/oph/oph-configuration/common.properties");
  }
}
