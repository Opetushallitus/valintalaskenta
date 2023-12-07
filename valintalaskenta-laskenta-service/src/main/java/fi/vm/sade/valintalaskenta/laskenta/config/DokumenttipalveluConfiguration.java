package fi.vm.sade.valintalaskenta.laskenta.config;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Profile("!dev")
@Configuration
public class DokumenttipalveluConfiguration {

  @Bean
  public Dokumenttipalvelu dokumenttipalvelu(
      @Value("aws.region") final String region, @Value("aws.bucket.name") final String bucketName) {
    return new Dokumenttipalvelu(region, bucketName);
  }
}
