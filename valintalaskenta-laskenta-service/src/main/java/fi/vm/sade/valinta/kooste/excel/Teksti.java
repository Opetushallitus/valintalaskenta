package fi.vm.sade.valinta.kooste.excel;

public class Teksti extends Solu {
  private final String teksti;
  private final int ulottuvuus;
  private final boolean tasausOikealle;
  private final boolean tasausVasemmalle;
  private final boolean lukittu;
  private final int preferoituleveys;
  private final boolean editoitava;

  public Teksti() {
    this.teksti = null;
    this.ulottuvuus = 1;
    this.tasausOikealle = false;
    this.tasausVasemmalle = true;
    this.lukittu = false;
    this.preferoituleveys = 0;
    this.editoitava = false;
  }

  public Teksti(String teksti) {
    this.teksti = teksti;
    this.ulottuvuus = 1;
    this.tasausOikealle = false;
    this.tasausVasemmalle = true;
    this.lukittu = false;
    this.preferoituleveys = 0;
    this.editoitava = false;
  }

  public Teksti(String teksti, boolean tasausOikealle, boolean lukittu) {
    this.teksti = teksti;
    this.ulottuvuus = 1;
    this.tasausOikealle = tasausOikealle;
    this.tasausVasemmalle = !tasausOikealle;
    this.lukittu = lukittu;
    this.preferoituleveys = 0;
    this.editoitava = false;
  }

  public Teksti(String teksti, boolean tasausOikealle, boolean lukittu, int preferoituleveys) {
    this.teksti = teksti;
    this.ulottuvuus = 1;
    this.tasausOikealle = tasausOikealle;
    this.tasausVasemmalle = !tasausOikealle;
    this.lukittu = lukittu;
    this.preferoituleveys = preferoituleveys;
    this.editoitava = false;
  }

  public Teksti(
      String teksti,
      boolean tasausOikealle,
      boolean tasausVasemmalle,
      boolean lukittu,
      int preferoituleveys,
      int ulottuvuus,
      boolean editoitava) {
    this.teksti = teksti;
    this.ulottuvuus = ulottuvuus;
    this.tasausOikealle = tasausOikealle;
    this.tasausVasemmalle = tasausVasemmalle;
    this.lukittu = lukittu;
    this.preferoituleveys = preferoituleveys;
    this.editoitava = editoitava;
  }

  @Override
  public boolean isLukittu() {
    return lukittu;
  }

  public Teksti(String teksti, int ulottuvuus) {
    this.teksti = teksti;
    this.ulottuvuus = ulottuvuus;
    this.tasausOikealle = false;
    this.tasausVasemmalle = true;
    this.lukittu = false;
    this.preferoituleveys = 0;
    this.editoitava = false;
  }

  @Override
  public boolean isMuokattava() {
    return editoitava;
  }

  @Override
  public int preferoituLeveys() {
    return preferoituleveys;
  }

  @Override
  public int ulottuvuus() {
    return ulottuvuus;
  }

  public String getTeksti() {
    return teksti;
  }

  public boolean isTeksti() {
    return true;
  }

  public Teksti toTeksti() {
    return this;
  }

  public Numero toNumero() {
    return new Numero();
  }

  public boolean isNumero() {
    return false;
  }

  @Override
  public boolean isTyhja() {
    return teksti == null;
  }

  @Override
  public String toString() {
    return "teksti: " + teksti;
  }

  private static final Teksti TYHJA = new Teksti();

  public static Teksti tyhja() {
    return TYHJA;
  }

  @Override
  public boolean isKeskitettyTasaus() {
    return tasausVasemmalle && tasausOikealle;
  }

  public boolean isTasausVasemmalle() {
    return tasausVasemmalle;
  }

  public boolean isTasausOikealle() {
    return tasausOikealle;
  }

  @Override
  public Monivalinta toMonivalinta() {
    throw new RuntimeException("Tekstiä ei voi muuttaa monivalintakentäksi!");
  }
}
