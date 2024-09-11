package fi.vm.sade.valinta.kooste.erillishaku.excel;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Collections;
import java.util.List;

@Schema
public class ErillishakuJson {
  private final List<ErillishakuRivi> rivit;

  public ErillishakuJson() {
    this.rivit = Collections.emptyList();
  }

  public ErillishakuJson(List<ErillishakuRivi> rivit) {
    this.rivit = rivit;
  }

  public List<ErillishakuRivi> getRivit() {
    return rivit;
  }
}
