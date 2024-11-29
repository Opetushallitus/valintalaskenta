package fi.vm.sade.valintalaskenta.laskenta;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;

@Profile("dev")
public class DevApp2 {

  private static final Logger LOG = LoggerFactory.getLogger(DevApp2.class);

  private static final String ENVIRONMENT = "untuva";

  public static void main(String[] args) {
    System.setProperty("server.port", "8081");
    DevApp.startApplication(args);
  }
}
