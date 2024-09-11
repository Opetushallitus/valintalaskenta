package fi.vm.sade.valinta.kooste.valintalaskenta.resource;

import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Maski;
import fi.vm.sade.valinta.seuranta.dto.LaskentaTyyppi;
import java.util.Optional;
import org.apache.commons.lang.StringUtils;

public class LaskentaParams {
  private final String userOID;
  private final String haunNimi;
  private final String nimi;
  private final LaskentaTyyppi laskentatyyppi;
  private final Boolean isValintakoelaskenta;
  private final Integer valinnanvaihe;
  private final String hakuOid;
  private final Optional<Maski> maski;
  private final boolean isErillishaku;

  public LaskentaParams(
      String userOID,
      String haunNimi,
      String nimi,
      LaskentaTyyppi laskentatyyppi,
      Boolean isValintakoelaskenta,
      Integer valinnanvaihe,
      String hakuOid,
      Optional<Maski> maski,
      boolean isErillishaku) {
    this.userOID = userOID;
    this.haunNimi = haunNimi;
    this.nimi = nimi;
    this.laskentatyyppi = laskentatyyppi;
    this.isValintakoelaskenta = isValintakoelaskenta;
    this.valinnanvaihe = valinnanvaihe;

    if (StringUtils.isBlank(hakuOid)) {
      throw new RuntimeException("HakuOid on pakollinen");
    }
    this.hakuOid = hakuOid;

    if (maski == null) {
      throw new RuntimeException("maski on pakollinen");
    }
    this.maski = maski;
    this.isErillishaku = isErillishaku;
  }

  public String getHaunNimi() {
    return haunNimi;
  }

  public String getNimi() {
    return nimi;
  }

  public String getUserOID() {
    return userOID;
  }

  public LaskentaTyyppi getLaskentatyyppi() {
    return laskentatyyppi;
  }

  public Boolean getIsValintakoelaskenta() {
    return isValintakoelaskenta;
  }

  public Integer getValinnanvaihe() {
    return valinnanvaihe;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public Optional<Maski> getMaski() {
    return maski;
  }

  public boolean isErillishaku() {
    return isErillishaku;
  }
}
