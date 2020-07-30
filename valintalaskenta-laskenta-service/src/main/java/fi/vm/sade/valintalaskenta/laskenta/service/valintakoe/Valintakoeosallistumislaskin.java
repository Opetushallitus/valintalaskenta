package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakukohde;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;

public interface Valintakoeosallistumislaskin {
  OsallistuminenTulos laskeOsallistuminenYhdelleHakukohteelle(
      Hakukohde hakukohde, Hakemus hakemus, Funktiokutsu kaava);
}
