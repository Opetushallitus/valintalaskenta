package fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class KoutaToteutus {
  public final String oid;
  public final String koulutusOid;
  public final Set<String> tarjoajat;
  public final Metadata metadata;

  private KoutaToteutus() {
    this.oid = null;
    this.koulutusOid = null;
    this.tarjoajat = null;
    this.metadata = null;
  }

  public String getKoulutuksenAlkamiskausi() {
    if (this.metadata != null
        && this.metadata.opetus != null
        && this.metadata.opetus.koulutuksenAlkamiskausi != null
        && this.metadata.opetus.koulutuksenAlkamiskausi.koulutuksenAlkamiskausi != null) {
      return this.metadata.opetus.koulutuksenAlkamiskausi.koulutuksenAlkamiskausi.koodiUri;
    }
    return null;
  }

  public Integer getKoulutuksenAlkamisvuosi() {
    if (this.metadata != null
        && this.metadata.opetus != null
        && this.metadata.opetus.koulutuksenAlkamiskausi != null) {
      return this.metadata.opetus.koulutuksenAlkamiskausi.koulutuksenAlkamisvuosi;
    }
    return null;
  }

  public Set<String> getOsaamisalaUris() {
    if (this.metadata != null && this.metadata.osaamisalat != null) {
      return this.metadata.osaamisalat.stream()
          .map(osaamisala -> osaamisala.koodi)
          .collect(Collectors.toSet());
    } else {
      return null;
    }
  }

  public static class Metadata {
    public final Opetus opetus;
    public final List<Osaamisala> osaamisalat;

    private Metadata() {
      this.opetus = null;
      this.osaamisalat = null;
    }
  }

  public static class Opetus {
    public final Set<String> opetuskieliKoodiUrit;
    public final KoulutuksenAlkamiskausi koulutuksenAlkamiskausi;

    private Opetus() {
      this.opetuskieliKoodiUrit = null;
      this.koulutuksenAlkamiskausi = null;
    }
  }

  public static class Osaamisala {
    public final String koodi;

    private Osaamisala() {
      this.koodi = null;
    }
  }

  public static class KoulutuksenAlkamiskausi {
    public final KoulutuksenAlkamiskausiKoodi koulutuksenAlkamiskausi;
    public final Integer koulutuksenAlkamisvuosi;

    private KoulutuksenAlkamiskausi() {
      this.koulutuksenAlkamiskausi = null;
      this.koulutuksenAlkamisvuosi = null;
    }
  }

  public static class KoulutuksenAlkamiskausiKoodi {
    public final String koodiUri;

    private KoulutuksenAlkamiskausiKoodi() {
      this.koodiUri = null;
    }
  }
}
