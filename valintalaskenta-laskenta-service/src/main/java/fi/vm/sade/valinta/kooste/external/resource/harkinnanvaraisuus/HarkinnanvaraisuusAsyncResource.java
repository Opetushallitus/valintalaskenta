package fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus;

import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.dto.HakemuksenHarkinnanvaraisuus;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Oppija;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface HarkinnanvaraisuusAsyncResource {

  CompletableFuture<List<HakemuksenHarkinnanvaraisuus>> getHarkinnanvaraisuudetForHakemukses(
      List<String> hakemusOids);

  // Kutsutaan atarun tiedoilla, palauttaa samanmuotoista dataa mutta synkattuna hakijan
  // sure-suoritusten kanssa.
  CompletableFuture<List<HakemuksenHarkinnanvaraisuus>> getSyncedHarkinnanvaraisuudes(
      List<HakemuksenHarkinnanvaraisuus> atarunTiedot);

  Boolean hasYksilollistettyMatAi(HakemuksenHarkinnanvaraisuus h, Oppija o);
}
