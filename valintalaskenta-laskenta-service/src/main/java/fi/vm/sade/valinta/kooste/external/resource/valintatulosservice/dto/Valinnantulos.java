package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import fi.vm.sade.sijoittelu.domain.HakemuksenTila;
import fi.vm.sade.sijoittelu.domain.IlmoittautumisTila;
import fi.vm.sade.sijoittelu.domain.ValintatuloksenTila;
import fi.vm.sade.sijoittelu.domain.dto.ErillishaunHakijaDTO;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.Optional;

public class Valinnantulos {
  private String hakukohdeOid;
  private String valintatapajonoOid;
  private String hakemusOid;
  private String henkiloOid;
  private Boolean ehdollisestiHyvaksyttavissa = null;
  private boolean julkaistavissa;
  private boolean hyvaksyttyVarasijalta;
  private boolean hyvaksyPeruuntunut;
  private Boolean poistettava = null;
  private Boolean ohitaVastaanotto = null;
  private Boolean ohitaIlmoittautuminen = null;
  private IlmoittautumisTila ilmoittautumistila;
  private HakemuksenTila valinnantila;
  private ValintatuloksenTila vastaanottotila;
  private String ehdollisenHyvaksymisenEhtoKoodi;
  private String ehdollisenHyvaksymisenEhtoFI;
  private String ehdollisenHyvaksymisenEhtoSV;
  private String ehdollisenHyvaksymisenEhtoEN;
  private String valinnantilanKuvauksenTekstiFI;
  private String valinnantilanKuvauksenTekstiSV;
  private String valinnantilanKuvauksenTekstiEN;
  private OffsetDateTime hyvaksymiskirjeLahetetty;

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    this.valintatapajonoOid = valintatapajonoOid;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public String getHenkiloOid() {
    return henkiloOid;
  }

  public void setHenkiloOid(String henkiloOid) {
    this.henkiloOid = henkiloOid;
  }

  public Boolean getEhdollisestiHyvaksyttavissa() {
    return ehdollisestiHyvaksyttavissa;
  }

  public void setEhdollisestiHyvaksyttavissa(Boolean ehdollisestiHyvaksyttavissa) {
    this.ehdollisestiHyvaksyttavissa = ehdollisestiHyvaksyttavissa;
  }

  public String getEhdollisenHyvaksymisenEhtoKoodi() {
    return ehdollisenHyvaksymisenEhtoKoodi;
  }

  public void setEhdollisenHyvaksymisenEhtoKoodi(String ehdollisenHyvaksymisenEhtoKoodi) {
    this.ehdollisenHyvaksymisenEhtoKoodi = ehdollisenHyvaksymisenEhtoKoodi;
  }

  public String getEhdollisenHyvaksymisenEhtoFI() {
    return ehdollisenHyvaksymisenEhtoFI;
  }

  public void setEhdollisenHyvaksymisenEhtoFI(String ehdollisenHyvaksymisenEhtoFI) {
    this.ehdollisenHyvaksymisenEhtoFI = ehdollisenHyvaksymisenEhtoFI;
  }

  public String getEhdollisenHyvaksymisenEhtoSV() {
    return ehdollisenHyvaksymisenEhtoSV;
  }

  public void setEhdollisenHyvaksymisenEhtoSV(String ehdollisenHyvaksymisenEhtoSV) {
    this.ehdollisenHyvaksymisenEhtoSV = ehdollisenHyvaksymisenEhtoSV;
  }

  public String getEhdollisenHyvaksymisenEhtoEN() {
    return ehdollisenHyvaksymisenEhtoEN;
  }

  public void setEhdollisenHyvaksymisenEhtoEN(String ehdollisenHyvaksymisenEhtoEN) {
    this.ehdollisenHyvaksymisenEhtoEN = ehdollisenHyvaksymisenEhtoEN;
  }

  public String getValinnantilanKuvauksenTekstiFI() {
    return valinnantilanKuvauksenTekstiFI;
  }

  public void setValinnantilanKuvauksenTekstiFI(String valinnantilanKuvauksenTekstiFI) {
    this.valinnantilanKuvauksenTekstiFI = valinnantilanKuvauksenTekstiFI;
  }

  public String getValinnantilanKuvauksenTekstiSV() {
    return valinnantilanKuvauksenTekstiSV;
  }

  public void setValinnantilanKuvauksenTekstiSV(String valinnantilanKuvauksenTekstiSV) {
    this.valinnantilanKuvauksenTekstiSV = valinnantilanKuvauksenTekstiSV;
  }

  public String getValinnantilanKuvauksenTekstiEN() {
    return valinnantilanKuvauksenTekstiEN;
  }

  public void setValinnantilanKuvauksenTekstiEN(String valinnantilanKuvauksenTekstiEN) {
    this.valinnantilanKuvauksenTekstiEN = valinnantilanKuvauksenTekstiEN;
  }

  public boolean getJulkaistavissa() {
    return julkaistavissa;
  }

  public void setJulkaistavissa(boolean julkaistavissa) {
    this.julkaistavissa = julkaistavissa;
  }

  public boolean getHyvaksyttyVarasijalta() {
    return hyvaksyttyVarasijalta;
  }

  public void setHyvaksyttyVarasijalta(boolean hyvaksyttyVarasijalta) {
    this.hyvaksyttyVarasijalta = hyvaksyttyVarasijalta;
  }

  public boolean getHyvaksyPeruuntunut() {
    return hyvaksyPeruuntunut;
  }

  public void setHyvaksyPeruuntunut(boolean hyvaksyPeruuntunut) {
    this.hyvaksyPeruuntunut = hyvaksyPeruuntunut;
  }

  public Boolean getPoistettava() {
    return poistettava;
  }

  public void setPoistettava(Boolean poistettava) {
    this.poistettava = poistettava;
  }

  public Boolean getOhitaVastaanotto() {
    return ohitaVastaanotto;
  }

  public void setOhitaVastaanotto(Boolean ohitaVastaanotto) {
    this.ohitaVastaanotto = ohitaVastaanotto;
  }

  public Boolean getOhitaIlmoittautuminen() {
    return ohitaIlmoittautuminen;
  }

  public void setOhitaIlmoittautuminen(Boolean ohitaIlmoittautuminen) {
    this.ohitaIlmoittautuminen = ohitaIlmoittautuminen;
  }

  public IlmoittautumisTila getIlmoittautumistila() {
    return ilmoittautumistila;
  }

  public void setIlmoittautumistila(IlmoittautumisTila ilmoittautumistila) {
    this.ilmoittautumistila = ilmoittautumistila;
  }

  public HakemuksenTila getValinnantila() {
    return valinnantila;
  }

  public void setValinnantila(HakemuksenTila valinnantila) {
    this.valinnantila = valinnantila;
  }

  public ValintatuloksenTila getVastaanottotila() {
    return vastaanottotila;
  }

  public void setVastaanottotila(ValintatuloksenTila vastaanottotila) {
    this.vastaanottotila = vastaanottotila;
  }

  public OffsetDateTime getHyvaksymiskirjeLahetetty() {
    return hyvaksymiskirjeLahetetty;
  }

  public void setHyvaksymiskirjeLahetetty(OffsetDateTime hyvaksymiskirjeLahetetty) {
    this.hyvaksymiskirjeLahetetty = hyvaksymiskirjeLahetetty;
  }

  public static Valinnantulos of(ErillishaunHakijaDTO hakija) {
    return of(hakija, null);
  }

  public static Valinnantulos of(ErillishaunHakijaDTO hakija, Boolean ohitaVastaanotto) {
    Valinnantulos valinnantulos = new Valinnantulos();
    valinnantulos.setHakemusOid(hakija.hakemusOid);
    valinnantulos.setHenkiloOid(hakija.hakijaOid);
    valinnantulos.setHakukohdeOid(hakija.hakukohdeOid);
    valinnantulos.setValintatapajonoOid(hakija.valintatapajonoOid);

    valinnantulos.setEhdollisestiHyvaksyttavissa(hakija.ehdollisestiHyvaksyttavissa);
    valinnantulos.setEhdollisenHyvaksymisenEhtoKoodi(hakija.ehdollisenHyvaksymisenEhtoKoodi);
    valinnantulos.setEhdollisenHyvaksymisenEhtoFI(hakija.ehdollisenHyvaksymisenEhtoFI);
    valinnantulos.setEhdollisenHyvaksymisenEhtoSV(hakija.ehdollisenHyvaksymisenEhtoSV);
    valinnantulos.setEhdollisenHyvaksymisenEhtoEN(hakija.ehdollisenHyvaksymisenEhtoEN);
    valinnantulos.setJulkaistavissa(hakija.julkaistavissa);
    valinnantulos.setHyvaksyttyVarasijalta(false);
    valinnantulos.setHyvaksyPeruuntunut(false);

    if (hakija.poistetaankoTulokset) {
      valinnantulos.setPoistettava(true);
    }

    if (null != ohitaVastaanotto && ohitaVastaanotto) {
      valinnantulos.setOhitaVastaanotto(true);
    }

    if (null == hakija.ilmoittautumisTila) {
      valinnantulos.setOhitaIlmoittautuminen(true);
    }
    if (null != hakija.getHyvaksymiskirjeLahetetty()) {
      valinnantulos.setHyvaksymiskirjeLahetetty(
          OffsetDateTime.ofInstant(
              hakija.getHyvaksymiskirjeLahetetty().toInstant(), ZoneId.of("Europe/Helsinki")));
    }

    valinnantulos.setValinnantila(hakija.hakemuksenTila);
    valinnantulos.setVastaanottotila(
        Optional.ofNullable(hakija.valintatuloksenTila).orElse(ValintatuloksenTila.KESKEN));
    valinnantulos.setIlmoittautumistila(
        Optional.ofNullable(hakija.ilmoittautumisTila).orElse(IlmoittautumisTila.EI_TEHTY));

    return valinnantulos;
  }

  @Override
  public String toString() {
    return "Valinnantulos{"
        + "hakukohdeOid='"
        + hakukohdeOid
        + '\''
        + ", valintatapajonoOid='"
        + valintatapajonoOid
        + '\''
        + ", hakemusOid='"
        + hakemusOid
        + '\''
        + ", henkiloOid='"
        + henkiloOid
        + '\''
        + ", ehdollisestiHyvaksyttavissa="
        + ehdollisestiHyvaksyttavissa
        + ", ehdollisenHyvaksymisenEhtoKoodi="
        + ehdollisenHyvaksymisenEhtoKoodi
        + ", ehdollisenHyvaksymisenEhtoFI="
        + ehdollisenHyvaksymisenEhtoFI
        + ", ehdollisenHyvaksymisenEhtoSV="
        + ehdollisenHyvaksymisenEhtoSV
        + ", ehdollisenHyvaksymisenEhtoEN="
        + ehdollisenHyvaksymisenEhtoEN
        + ", valinnantilanKuvauksenTekstiFI="
        + valinnantilanKuvauksenTekstiFI
        + ", valinnantilanKuvauksenTekstiSV="
        + valinnantilanKuvauksenTekstiSV
        + ", valinnantilanKuvauksenTekstiEN="
        + valinnantilanKuvauksenTekstiEN
        + ", julkaistavissa="
        + julkaistavissa
        + ", hyvaksyttyVarasijalta="
        + hyvaksyttyVarasijalta
        + ", hyvaksyPeruuntunut="
        + hyvaksyPeruuntunut
        + ", poistettava="
        + poistettava
        + ", ohitaVastaanotto="
        + ohitaVastaanotto
        + ", ohitaIlmoittautuminen="
        + ohitaIlmoittautuminen
        + ", ilmoittautumistila="
        + ilmoittautumistila
        + ", valinnantila="
        + valinnantila
        + ", vastaanottotila="
        + vastaanottotila
        + ", hyvaksymiskirjeLahetetty="
        + hyvaksymiskirjeLahetetty
        + '}';
  }
}
