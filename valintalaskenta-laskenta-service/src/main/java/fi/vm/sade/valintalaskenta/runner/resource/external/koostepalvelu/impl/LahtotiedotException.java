package fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.impl;

import fi.vm.sade.valintalaskenta.runner.resource.external.RunnerRestCasClient;

public class LahtotiedotException extends RuntimeException {

  private static String getMessage(Throwable cause) {
    if (cause instanceof RunnerRestCasClient.RestCasClientException) {
      RunnerRestCasClient.RestCasClientException e =
          (RunnerRestCasClient.RestCasClientException) cause;
      if (e.getResponse().hasResponseBody()) {
        return e.getResponse().getResponseBody();
      }
      return "Lähtötietojen haku epäonnistui, http-paluuarvo: "
          + e.getResponse().getStatusCode()
          + " "
          + e.getResponse().getStatusText();
    }
    return cause.getMessage();
  }

  private static Throwable getUnderlyingCause(Throwable t) {
    if (t.getCause() != null) {
      return getUnderlyingCause(t.getCause());
    }
    return t;
  }

  public LahtotiedotException(Throwable cause) {
    super(getMessage(getUnderlyingCause(cause)), cause);
  }
}
