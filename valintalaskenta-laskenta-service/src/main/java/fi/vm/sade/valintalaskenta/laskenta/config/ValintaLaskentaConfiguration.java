package fi.vm.sade.valintalaskenta.laskenta.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;

@Configuration
@ComponentScan(basePackages = "fi.vm.sade.valintalaskenta")
@ImportResource({"classpath:spring/application-context.xml"})
public class ValintaLaskentaConfiguration {}
