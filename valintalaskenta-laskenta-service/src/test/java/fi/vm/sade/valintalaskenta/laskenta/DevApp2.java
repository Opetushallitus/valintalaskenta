package fi.vm.sade.valintalaskenta.laskenta;

import org.springframework.context.annotation.Profile;

@Profile("dev")
public class DevApp2 {

  public static void main(String[] args) {
    System.setProperty("server.port", "8081");
    DevApp.startApplication(args);
  }
}
