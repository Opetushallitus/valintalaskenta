package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

public abstract class AbstractValintakoe {
  public final String id;
  public final String valintakokeentyyppiUri;

  public AbstractValintakoe(String id, String valintakokeentyyppiUri) {
    this.id = id;
    this.valintakokeentyyppiUri = valintakokeentyyppiUri;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    AbstractValintakoe that = (AbstractValintakoe) o;

    return id.equals(that.id);
  }

  @Override
  public int hashCode() {
    return id.hashCode();
  }
}
