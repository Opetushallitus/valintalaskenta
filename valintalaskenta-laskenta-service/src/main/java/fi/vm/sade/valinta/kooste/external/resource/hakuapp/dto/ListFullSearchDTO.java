package fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto;

import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.List;

public class ListFullSearchDTO {
  private final String searchTerms;
  private final List<String> aoOids;
  private final List<String> asIds;
  private final List<String> states;
  private final List<String> keys;

  @JsonCreator
  public ListFullSearchDTO(
      String searchTerms,
      List<String> aoOids,
      List<String> asIds,
      List<String> states,
      List<String> keys) {
    this.searchTerms = searchTerms;
    this.aoOids = aoOids;
    this.asIds = asIds;
    this.states = states;
    this.keys = keys;
  }

  public String getSearchTerms() {
    return searchTerms;
  }

  public List<String> getAoOids() {
    return aoOids;
  }

  public List<String> getAsIds() {
    return asIds;
  }

  public List<String> getStates() {
    return states;
  }

  public List<String> getKeys() {
    return keys;
  }
}
