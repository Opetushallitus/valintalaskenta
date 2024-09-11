package fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice;

import fi.vm.sade.sijoittelu.domain.ValintatuloksenTila;
import fi.vm.sade.sijoittelu.domain.Valintatulos;
import fi.vm.sade.sijoittelu.domain.dto.ErillishaunHakijaDTO;
import jakarta.validation.constraints.NotNull;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;

public class VastaanottoRecordDTO {
  private String valintatapajonoOid;
  private String henkiloOid;
  private String hakemusOid;
  private String hakuOid;
  private String hakukohdeOid;
  private String ilmoittaja;
  private ValintatuloksenTila tila;
  private String selite;

  private static final Logger LOG = LoggerFactory.getLogger(VastaanottoRecordDTO.class);

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public String getHenkiloOid() {
    return henkiloOid;
  }

  public void setHenkiloOid(String henkiloOid) {
    assertNotNull(henkiloOid, "henkiloOid");
    this.henkiloOid = henkiloOid;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    assertNotNull(hakemusOid, "hakemusOid");
    this.hakemusOid = hakemusOid;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    assertNotNull(hakuOid, "hakuOid");
    this.hakuOid = hakuOid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    assertNotNull(hakukohdeOid, "hakukohdeOid");
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getIlmoittaja() {
    return ilmoittaja;
  }

  public void setIlmoittaja(String ilmoittaja) {
    assertNotNull(ilmoittaja, "ilmoittaja");
    this.ilmoittaja = ilmoittaja;
  }

  public ValintatuloksenTila getTila() {
    return tila;
  }

  public void setTila(ValintatuloksenTila tila) {
    assertNotNull(tila, "tila");
    this.tila = tila;
  }

  public String getSelite() {
    return selite;
  }

  public void setSelite(@NotNull String selite) {
    assertNotNull(selite, "selite");
    this.selite = selite;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    assertNotNull(valintatapajonoOid, "valintatapajonoOid");
    this.valintatapajonoOid = valintatapajonoOid;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }

  private void assertNotNull(Object value, String name) {
    Assert.notNull(value, name + " may not be null");
  }

  public static VastaanottoRecordDTO of(
      Valintatulos valintatulos, String muokkaaja, String selite) {
    try {
      VastaanottoRecordDTO v = new VastaanottoRecordDTO();
      v.setValintatapajonoOid(valintatulos.getValintatapajonoOid());
      v.setHenkiloOid(valintatulos.getHakijaOid());
      v.setHakemusOid(valintatulos.getHakemusOid());
      v.setHakuOid(valintatulos.getHakuOid());
      v.setHakukohdeOid(valintatulos.getHakukohdeOid());
      v.setIlmoittaja(muokkaaja);
      v.setTila(valintatulos.getTila());
      v.setSelite(selite);
      return v;
    } catch (IllegalArgumentException e) {
      LOG.error(
          String.format(
              "Problem when creating %s from valintatulos %s",
              VastaanottoRecordDTO.class.getSimpleName(), valintatulos),
          e);
      throw e;
    }
  }

  public static VastaanottoRecordDTO of(
      ErillishaunHakijaDTO hakija, String muokkaaja, String selite) {
    try {
      VastaanottoRecordDTO dto = new VastaanottoRecordDTO();
      dto.setValintatapajonoOid(hakija.getValintatapajonoOid());
      dto.setHakemusOid(hakija.getHakemusOid());
      dto.setHakukohdeOid(hakija.getHakukohdeOid());
      dto.setHakuOid(hakija.getHakuOid());
      dto.setHenkiloOid(hakija.getHakijaOid());
      dto.setIlmoittaja(muokkaaja);
      dto.setSelite(selite);
      dto.setTila(hakija.getValintatuloksenTila());
      return dto;
    } catch (IllegalArgumentException e) {
      LOG.error(
          String.format(
              "Problem when creating %s from erillishaun hakija %s",
              VastaanottoRecordDTO.class.getSimpleName(), hakija),
          e);
      throw e;
    }
  }
}
