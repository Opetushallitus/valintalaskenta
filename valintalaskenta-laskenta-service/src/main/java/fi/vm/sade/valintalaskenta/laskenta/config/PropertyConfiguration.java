package fi.vm.sade.valintalaskenta.laskenta.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.context.annotation.PropertySource;

@Profile({"!dev & !test & !test-mockless"})
@Configuration
@PropertySource(value = {
    "file:///${user.home:''}/oph-configuration/application.properties"
})
public class PropertyConfiguration {
}
