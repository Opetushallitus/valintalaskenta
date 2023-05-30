package fi.vm.sade.valintalaskenta.laskenta.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfiguration {
  @Bean
  public OpenAPI valintalaskentaAPI() {
    return new OpenAPI()
        .info(
            new Info().title("Valintalaskenta API").description("Valintalaskenta").version("v1.0"));
  }
}
