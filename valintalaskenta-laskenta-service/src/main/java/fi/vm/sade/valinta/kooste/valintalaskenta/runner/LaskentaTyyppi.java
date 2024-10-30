package fi.vm.sade.valinta.kooste.valintalaskenta.runner;

/**
 * Laskentatyyppi määrittää minkä tyyppistä laskentaa ollaan tekemässä.
 */
public enum LaskentaTyyppi {

  /**
   * Valintakoelaskentaa käytetään tilanteessa jossa halutaan laskea ketkä hakijat tulevat ennakkotehtävien
   * perusteella kutsutuksi valintakokeeseen.
   */
  VALINTAKOELASKENTA,

  /**
   * Tätä laskentatyyppiä käytetään kun halutaan laskea yhden hakukohteen kaikki valinnanvaiheet.
   */
  KAIKKI,

  /**
   * Tätä laskentatyyppiä käytetään kun halutaan laskea yhden hakukohteen yksi valinnanvaihe
   */
  VALINTALASKENTA,

  /**
   * Valintaryhmälaskentaa käytetään tilanteissa jossa useammalla hakukohteella on sama valintakoe. Tällöin
   * Valintaryhmälaskenta laskee ketkä tulevat kutsutuksi kokeeseen. Valintaryhmä voidaan myös muodostaa tilanteissa
   * joissa useampi hakukohde halutaan syystä tai toisesta laskea yhdessä (esim. yhteishaun kaikki sote-hakukohteet).
   */
  VALINTARYHMALASKENTA
}
