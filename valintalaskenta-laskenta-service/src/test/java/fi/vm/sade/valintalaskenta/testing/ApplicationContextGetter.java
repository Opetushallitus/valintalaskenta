package fi.vm.sade.valintalaskenta.testing;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

public class ApplicationContextGetter implements ApplicationContextAware {
  public static ApplicationContext applicationContext;

  @Override
  public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
    ApplicationContextGetter.applicationContext = applicationContext;
  }
}
