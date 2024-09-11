package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import com.google.common.hash.BloomFilter;
import com.google.common.hash.Funnels;
import java.nio.charset.Charset;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HakukohdeLaskuri extends Laskuri {
  private static final Logger LOG = LoggerFactory.getLogger(HakukohdeLaskuri.class);
  private final BloomFilter<CharSequence> duplicateChecker;

  public HakukohdeLaskuri(int hakukohteita) {
    super(hakukohteita);
    this.duplicateChecker =
        BloomFilter.create(Funnels.stringFunnel(Charset.forName("UTF-8")), hakukohteita);
  }

  public boolean done(String hakukohdeOid) {
    try {
      if (!duplicateChecker.put(hakukohdeOid)) {
        LOG.error(
            "Hakukohde {} saattoi olla jo merkittyna valmiiksi! Havainto oli virheellinen noin 3% todennakoisyydella!",
            hakukohdeOid);
      }
    } catch (Exception e) {
      LOG.error("Bloomfilterin kutsu epaonnistui!", e);
    }
    int l = tiputaLaskuria();
    if (l < 0) {
      int yhteensa = getYhteensa();
      LOG.error(
          "Hakukohteita merkitty valmiiksi odotettua enemman! {}/{} eli ylimaaraisia merkintoja on {}",
          (-l) + yhteensa,
          yhteensa,
          -l);
      return false;
    }
    boolean ready = l == 0;
    return ready;
  }
}
