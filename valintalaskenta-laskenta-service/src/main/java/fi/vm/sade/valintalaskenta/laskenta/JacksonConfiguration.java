package fi.vm.sade.valintalaskenta.laskenta;

import com.fasterxml.jackson.core.StreamReadConstraints;
import org.springframework.boot.autoconfigure.jackson.Jackson2ObjectMapperBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class JacksonConfiguration {
  @Bean
  Jackson2ObjectMapperBuilderCustomizer customStreamReadConstraints() {
    return (builder) ->
        builder.postConfigurer(
            (objectMapper) ->
                objectMapper
                    .getFactory()
                    .setStreamReadConstraints(
                        StreamReadConstraints.builder().maxStringLength(10000000).build()));
  }
}
