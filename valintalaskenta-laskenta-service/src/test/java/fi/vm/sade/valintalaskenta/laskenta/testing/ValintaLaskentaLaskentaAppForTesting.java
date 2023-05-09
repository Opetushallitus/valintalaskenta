package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.integrationtest.util.ProjectRootFinder;
import fi.vm.sade.valintalaskenta.laskenta.ValintaLaskentaLaskentaApplication;
import org.springframework.beans.BeansException;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.ImportResource;

@SpringBootApplication
@ImportResource({"classpath:application-context-http-test.xml"})
public class ValintaLaskentaLaskentaAppForTesting {
  public static void main(String[] args) {
    ValintaLaskentaLaskentaApplication.main(args);
  }

  public static class ApplicationContextGetter implements ApplicationContextAware {
    public static ApplicationContext applicationContext;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
      ApplicationContextGetter.applicationContext = applicationContext;
    }
  }
}
