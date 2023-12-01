package fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvoContainer;
import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class SijoitteluMorko {

  public int aloituspaikat;

  public Boolean eiVarasijatayttoa;

  public Boolean kaytetaanValintalaskentaa;

  public Boolean kaikkiEhdonTayttavatHyvaksytaan;

  public Boolean poissaOlevaTaytto;

  public String valintatapajonoOid;

  public UUID id;

  public int prioriteetti;

  public Boolean siirretaanSijoitteluun;

  public Tasasijasaanto tasasijasaanto;

  public Boolean valmisSijoiteltavaksi;

  public Long sijoitteluajoId;

  public Boolean kaytetaanKokonaisPisteita;

  public String nimi;

  // Valinnanvaihe fields
  public Date createdAt;

  public String valinnanvaiheOid;

  public String valinnanvaiheNimi;

  public int jarjestysnumero;

  // Jonosijan tiedot

  public UUID jonosijaId;

  public String hakemusOid;

  public String hakijaOid;

  public SyotettyArvoContainer syotetytArvot;

  public int hakutoiveprioriteetti;

  public Boolean harkinnanvarainen;

  public Boolean hylattyValisijoittelussa;

  // Jarjestyskriteerin tiedot:
  public JarjestyskriteerituloksenTila tila;

  public BigDecimal arvo;

  public int kriteeriPrioriteetti;

  public String kriteeriNimi;

  public String kuvausFi;

  public String kuvausSv;

  public String kuvausEn;

  public Map<String, String> getKuvaus() {
    Map<String, String> kuvaus = new HashMap<>();
    if (kuvausFi != null) {
      kuvaus.put("FI", kuvausFi);
    }
    if (kuvausSv != null) {
      kuvaus.put("SV", kuvausSv);
    }
    if (kuvausEn != null) {
      kuvaus.put("EN", kuvausEn);
    }
    return kuvaus;
  }
}
