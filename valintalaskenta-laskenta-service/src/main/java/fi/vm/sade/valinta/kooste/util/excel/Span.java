package fi.vm.sade.valinta.kooste.util.excel;

public class Span {
  private final String text;
  private final int spanColumns;
  private final boolean alsoHighlight;

  public Span(String text, int spanColumns) {
    this.text = text;
    this.spanColumns = spanColumns;
    this.alsoHighlight = false;
  }

  public Span(String text, int spanColumns, boolean alsoHighlight) {
    this.text = text;
    this.spanColumns = spanColumns;
    this.alsoHighlight = alsoHighlight;
  }

  public boolean isAlsoHighlight() {
    return alsoHighlight;
  }

  public int getSpanColumns() {
    return spanColumns;
  }

  public String getText() {
    return text;
  }
}
