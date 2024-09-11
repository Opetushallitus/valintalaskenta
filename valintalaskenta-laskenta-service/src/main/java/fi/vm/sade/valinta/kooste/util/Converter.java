package fi.vm.sade.valinta.kooste.util;

import fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto.Eligibility;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.Valintapisteet;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import java.util.*;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Converter {
  private static final Logger LOG = LoggerFactory.getLogger(Converter.class);
  private static final String EI_ARVOSANAA = "Ei arvosanaa";

  public static class Hakutoive {
    private Boolean harkinnanvaraisuus;
    private String hakukohdeOid;

    Boolean getHarkinnanvaraisuus() {
      return harkinnanvaraisuus;
    }

    void setHarkinnanvaraisuus(Boolean harkinnanvaraisuus) {
      this.harkinnanvaraisuus = harkinnanvaraisuus;
    }

    public String getHakukohdeOid() {
      return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
      this.hakukohdeOid = hakukohdeOid;
    }
  }

  /**
   * Poistaa "Ei arvosanaa" -kentät hakemukselta. Tämän funkkarin voi poistaa kunhan
   * hakemuspalveluun saadaan tehtyä filtteri näille kentille
   */
  static String sanitizeArvo(String arvo) {
    if (arvo != null && EI_ARVOSANAA.equals(arvo)) {
      return "";
    }

    return arvo;
  }

  static void setHakemusDTOvalintapisteet(Valintapisteet valintapisteet, HakemusDTO hakemusDto) {
    try {
      if (valintapisteet != null) {
        Valintapisteet.toAdditionalData(valintapisteet)
            .getAdditionalData()
            .forEach(
                (k, v) -> {
                  AvainArvoDTO aa = new AvainArvoDTO();
                  aa.setAvain(k);
                  aa.setArvo(v);
                  hakemusDto.getAvaimet().add(aa);
                });
      }
    } catch (Exception e) {
      LOG.error("Epäonnistuminen hakemuksen valintapisteiden asettamisessa", e);
      throw e;
    }
  }

  public static Map<String, String> mapEligibilityAndStatus(
      List<Eligibility> eligibilities, Map<String, String> hakutoiveet) {
    Map<String, String> eligibilityAndStatus =
        Optional.ofNullable(eligibilities).orElse(Collections.emptyList()).stream()
            .filter(Objects::nonNull)
            // .map(e -> e.getAoId())
            .collect(Collectors.toList())
            .stream()
            .collect(
                Collectors.toMap(Eligibility::getAoId, Eligibility::getParsedEligibilityStatus));
    return Optional.ofNullable(hakutoiveet).orElse(Collections.emptyMap()).entrySet().stream()
        // preference{x}-Koulutus-id eli esim preference2-Koulutus-id
        .filter(
            pair -> {
              boolean b =
                  pair.getKey().startsWith("preference") && pair.getKey().endsWith("-Koulutus-id");
              LOG.debug("Matsaako {} {}", pair, b);
              return b;
            })
        // eligibility with aoId exists
        .filter(
            pair -> {
              boolean b = eligibilityAndStatus.containsKey(pair.getValue());
              LOG.debug("Matsaako key({}) == {}", pair.getValue(), b);
              return b;
            })
        // Maps
        // preference2-Koulutus-id = "1.2.246.562.20.645785477510"
        // To
        // preference2-Koulutus-id-eligibility = "UNKNOWN"
        .collect(
            Collectors.toMap(
                pair -> pair.getKey() + "-eligibility",
                pair -> eligibilityAndStatus.get(pair.getValue())));
  }
}
