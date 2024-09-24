package fi.vm.sade.valinta.kooste.external.resource.ataru;

import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface AtaruAsyncResource {

  /**
   *
   * @param hakukohdeOid                  Hakukohteen tunniste
   * @param withHarkinnanvaraisuustieto   Sisällytetäänkö harkinnanvaraisuustieto. Harkinnanvaraisuustiedon haku on
   *                                      raskas operaatio, joten se on syytä sisällyttää vain tarvittaessa.
   * @return
   */
  CompletableFuture<List<HakemusWrapper>> getApplicationsByHakukohde(
      String hakukohdeOid, boolean withHarkinnanvaraisuustieto);
}
