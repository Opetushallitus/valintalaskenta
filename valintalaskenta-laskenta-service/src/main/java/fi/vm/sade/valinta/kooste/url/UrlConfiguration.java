package fi.vm.sade.valinta.kooste.url;

import fi.vm.sade.properties.OphProperties;
import java.nio.file.Paths;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UrlConfiguration extends OphProperties {

  private static final Logger LOG = LoggerFactory.getLogger(UrlConfiguration.class);
  private static volatile UrlConfiguration instance;

  protected UrlConfiguration() {
    addFiles("/valintalaskentakoostepalvelu-oph.properties");
    addOptionalFiles(
        Paths.get(
                System.getProperties().getProperty("user.home"),
                "/oph-configuration/common.properties")
            .toString());
  }

  @Override
  public String url(String key, Object... params) {
    try {
      return super.url(key, params);
    } catch (Exception e) {
      LOG.error(e.getMessage(), e);
      throw e;
    }
  }

  /**
   * Get instance of UrlConfiguration.
   *
   * <p>This method initializes a new instance of UrlConfiguration, if one has not yet been
   * initialized. Initialization occurs lazily and uses double checked locking for thread safety
   * (see Joshua Bloch: Effective Java, Item 71: Use lazy initialization judiciously).
   *
   * @return UrlConfiguration
   */
  public static UrlConfiguration getInstance() {
    UrlConfiguration i = instance;
    if (i == null) {
      synchronized (UrlConfiguration.class) {
        i = instance;
        if (i == null) {
          instance = i = new UrlConfiguration();
        }
      }
    }
    return i;
  }
}
