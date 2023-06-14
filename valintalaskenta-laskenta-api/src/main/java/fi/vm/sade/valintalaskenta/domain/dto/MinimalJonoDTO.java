package fi.vm.sade.valintalaskenta.domain.dto;

import com.google.common.base.Preconditions;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Schema(
    name = "valintalaskenta.domain.dto.MinimalJonoDTO",
    description = "Valintatapajono, jossa vain pakolliset kentät ODWlle")
public class MinimalJonoDTO {
  private static final Logger LOGGER = LoggerFactory.getLogger(MinimalJonoDTO.class);

  @Schema(title = "Haku OID")
  private String hakuOid;

  @Schema(title = "Hakukohde OID")
  private String hakukohdeOid;

  @Schema(title = "Valintatapajono OID")
  private String valintatapajonoOid;

  @Schema(title = "Hakemusten määrä jonossa")
  private int hakemusCount;

  @Schema(title = "Jono käyttää valintalaskentaa")
  private final boolean kaytetaanValintalaskentaa;

  @Schema(title = "Jono on siirretty sijoitteluun")
  private final boolean siirretaanSijoitteluun;

  public MinimalJonoDTO(
      String hakuOid,
      String hakukohdeOid,
      String valintatapajonoOid,
      List<JonosijaDTO> jonosijaDTOS,
      Boolean kaytetaanValintalaskentaa,
      Boolean siirretaanSijoitteluun) {
    Preconditions.checkNotNull(hakuOid);
    Preconditions.checkNotNull(hakukohdeOid);
    Preconditions.checkNotNull(valintatapajonoOid);
    this.hakuOid = hakuOid;
    this.hakukohdeOid = hakukohdeOid;
    this.valintatapajonoOid = valintatapajonoOid;
    this.kaytetaanValintalaskentaa = Boolean.TRUE.equals(kaytetaanValintalaskentaa);
    this.siirretaanSijoitteluun = Boolean.TRUE.equals(siirretaanSijoitteluun);

    if (jonosijaDTOS == null) {
      LOGGER.warn("Valintatapajono {} had null jonosijas.", valintatapajonoOid);
      this.hakemusCount = 0;
    } else {
      this.hakemusCount = jonosijaDTOS.size();
    }
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public int getHakemusCount() {
    return hakemusCount;
  }

  public boolean isKaytetaanValintalaskentaa() {
    return kaytetaanValintalaskentaa;
  }

  public boolean isSiirretaanSijoitteluun() {
    return siirretaanSijoitteluun;
  }
}
