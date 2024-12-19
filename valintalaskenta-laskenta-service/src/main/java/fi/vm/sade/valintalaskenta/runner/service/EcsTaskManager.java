package fi.vm.sade.valintalaskenta.runner.service;

public interface EcsTaskManager {

  void withTaskProtection(Runnable runnable);

  String getTaskArn();
}
