package fi.vm.sade.valinta.kooste.valintalaskenta.dto;

import com.google.common.collect.Sets;
import fi.vm.sade.valinta.kooste.valintalaskenta.actor.dto.HakukohdeJaOrganisaatio;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Maski osittaiseen laskentaan haulle */
public class Maski {
  private static final Logger LOG = LoggerFactory.getLogger(Maski.class);
  private final Collection<String> hakukohteet;
  private final boolean whiteList;

  public Maski() {
    this.whiteList = false;
    this.hakukohteet = null;
  }

  private Maski(boolean whitelist, Collection<String> hakukohteet) {
    this.whiteList = whitelist;
    this.hakukohteet = hakukohteet;
  }

  public boolean isMask() {
    return hakukohteet != null && !hakukohteet.isEmpty();
  }

  public static Maski whitelist(Collection<String> hakukohteet) {
    return new Maski(true, hakukohteet);
  }

  public static Maski blacklist(Collection<String> hakukohteet) {
    return new Maski(false, hakukohteet);
  }

  public boolean isBlacklist() {
    return !whiteList && isMask();
  }

  public boolean isWhitelist() {
    return whiteList && isMask();
  }

  public Collection<HakukohdeJaOrganisaatio> maskaa(
      Collection<HakukohdeJaOrganisaatio> originalHjaO) {
    Set<String> lopulliset = Collections.emptySet();
    Set<String> original =
        originalHjaO.stream()
            .map(HakukohdeJaOrganisaatio::getHakukohdeOid)
            .collect(Collectors.toSet());
    Set<String> hakukohdeOidsMask = Sets.newHashSet(hakukohteet);
    if (isMask()) {
      if (isWhitelist()) {
        lopulliset =
            applyWhitelist(
                hakukohdeOidsMask,
                original,
                (s ->
                    LOG.error(
                        "Haku ei taysin vastaa syotetyn whitelistin hakukohteita! Puuttuvat hakukohteet \r\n{}",
                        Arrays.toString(s.toArray()))));
      } else if (isBlacklist()) {
        lopulliset =
            applyBlacklist(
                hakukohdeOidsMask,
                original,
                (s ->
                    LOG.error(
                        "Haku ei taysin vastaa syotetyn blacklistin hakukohteita! Ylimaaraiset hakukohteet \r\n{}",
                        Arrays.toString(s.toArray()))));
      }
    }
    final Set<String> lopullisetFilter = lopulliset;
    return originalHjaO.stream()
        .filter(hk -> lopullisetFilter.contains(hk.getHakukohdeOid()))
        .collect(Collectors.toList());
  }

  public String toString() {
    String maskTypeAsString = isWhitelist() ? "Whitelist" : "Blacklist";
    String hakukohteetAsString =
        hakukohteet != null ? Arrays.toString(hakukohteet.toArray()) : StringUtils.EMPTY;
    return String.format(maskTypeAsString + " Maski {}", hakukohteetAsString);
  }

  private <T> Set<T> applyWhitelist(Set<T> w, Set<T> t, Consumer<Set<T>> ylimaaraiset) {
    if (t.containsAll(w)) {
      return w;
    } else {
      Set<T> s = Sets.newHashSet(w);
      s.removeAll(t);
      ylimaaraiset.accept(s);
      Set<T> copy = Sets.newHashSet(w);
      copy.removeAll(s);
      return copy;
    }
  }

  private <T> Set<T> applyBlacklist(Set<T> b, Set<T> t, Consumer<Collection<T>> ylimaaraiset) {
    Set<T> copy = Sets.newHashSet(t);
    copy.removeAll(b);
    if (t.containsAll(b)) {
      return copy;
    } else {
      Set<T> s = Sets.newHashSet(b);
      s.removeAll(t);
      ylimaaraiset.accept(s);
      return copy;
    }
  }
}
