package fi.vm.sade.valinta.kooste.external.resource.ataru.dto;

import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.HarkinnanvaraisuudenSyy;

public class AtaruHakutoive {
  private String processingState;
  private String eligibilityState;
  private String paymentObligation;
  private String hakukohdeOid;
  private String languageRequirement;
  private String degreeRequirement;
  private HarkinnanvaraisuudenSyy harkinnanvaraisuus;

  public String getProcessingState() {
    return processingState;
  }

  public void setProcessingState(String processingState) {
    this.processingState = processingState;
  }

  public String getEligibilityState() {
    return eligibilityState;
  }

  public void setEligibilityState(String eligibilityState) {
    this.eligibilityState = eligibilityState;
  }

  public String getPaymentObligation() {
    return paymentObligation;
  }

  public void setPaymentObligation(String paymentObligation) {
    this.paymentObligation = paymentObligation;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getLanguageRequirement() {
    return languageRequirement;
  }

  public void setLanguageRequirement(String languageRequirement) {
    this.languageRequirement = languageRequirement;
  }

  public String getDegreeRequirement() {
    return degreeRequirement;
  }

  public void setDegreeRequirement(String degreeRequirement) {
    this.degreeRequirement = degreeRequirement;
  }

  public HarkinnanvaraisuudenSyy getHarkinnanvaraisuus() {
    if (harkinnanvaraisuus == null) {
      return HarkinnanvaraisuudenSyy.EI_HARKINNANVARAINEN;
    }
    return harkinnanvaraisuus;
  }

  public void setHarkinnanvaraisuus(HarkinnanvaraisuudenSyy harkinnanvaraisuus) {
    this.harkinnanvaraisuus = harkinnanvaraisuus;
  }
}
