package fi.vm.sade.valinta.kooste.util;

import fi.vm.sade.valinta.kooste.external.resource.ataru.dto.AtaruHakutoive;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.Valintapisteet;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public abstract class HakemusWrapper {
  static String NAINEN = "2";
  static String MIES = "1";

  public abstract String getOid();

  public abstract String getUlkomainenLahiosoite();

  public abstract String getSukupuoli();

  public abstract String getSukupuoliAsIs();

  public abstract String getAidinkieli();

  public abstract String getKaupunkiUlkomaa();

  public abstract String getUlkomainenPostinumero();

  public abstract String getUlkomainenPostitoimipaikka();

  public abstract String getSuomalainenLahiosoite();

  public abstract String getSuomalainenPostinumero();

  public abstract String getAsuinmaa();

  public abstract String getKansallinenId();

  public abstract String getKansalaisuus();

  public abstract String getPassinnumero();

  public abstract String getKotikunta();

  public abstract String getPuhelinnumero();

  public abstract Collection<String> getPuhelinnumerot();

  public abstract boolean isMaksuvelvollinen(String hakukohdeOid);

  public abstract String getSahkopostiOsoite();

  public abstract String getSyntymaaika();

  public abstract String getSyntymaaikaForErillishaku();

  public abstract String getHenkilotunnus();

  public abstract boolean hasHenkilotunnus();

  public abstract String getPersonOid();

  public abstract String getApplicationPersonOid();

  public abstract Integer getHakutoiveenPrioriteetti(String hakukohdeOid);

  public abstract Boolean getToisenAsteenSuoritus();

  public abstract String getToisenAsteenSuoritusmaa();

  public abstract String getEtunimi();

  public abstract String getKutsumanimi();

  public abstract String getEtunimet();

  public abstract String getSukunimi();

  public abstract boolean getLupaJulkaisuun();

  public abstract boolean getVainSahkoinenViestinta();

  public abstract boolean hasAsiointikieli();

  public abstract String getAsiointikieli();

  public abstract boolean getLupaSahkoiseenAsiointiin();

  public abstract boolean getLupaTulosEmail();

  public abstract Collection<String> getHakutoiveOids();

  public abstract String getMaksuvelvollisuus(String hakukohdeOid);

  public abstract boolean ulkomaillaSuoritettuKoulutusTaiOppivelvollisuudenKeskeyttanyt();

  public abstract String getHakuoid();

  public abstract String getState();

  public abstract int hashCode();

  public abstract boolean equals(Object o);

  public abstract HakemusDTO toHakemusDto(
      Valintapisteet valintapisteet,
      Map<String, List<String>> hakukohdeRyhmasForHakukohdes,
      boolean shouldUseApplicationPersonOid);

  public abstract List<AtaruHakutoive> ataruHakutoiveet();

  public abstract List<String> getHuoltajienSahkopostiosoitteet();

  public abstract String getSyntymapaikka();
}
