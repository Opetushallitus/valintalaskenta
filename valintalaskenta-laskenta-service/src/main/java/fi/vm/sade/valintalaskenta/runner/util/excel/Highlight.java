package fi.vm.sade.valintalaskenta.runner.util.excel;

import org.apache.commons.lang.StringUtils;

public class Highlight {
  private final Object text;

  public Highlight(Object text) {
    this.text = text;
  }

  @Override
  public String toString() {
    if (text == null) {
      return StringUtils.EMPTY;
    }
    return text.toString();
  }
}
