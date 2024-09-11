package fi.vm.sade.valinta.kooste.excel;

public class ExcelValidointiPoikkeus extends RuntimeException {

  public ExcelValidointiPoikkeus(String virhe) {
    super(virhe);
  }

  private static final long serialVersionUID = 8808887945396811916L;
}
