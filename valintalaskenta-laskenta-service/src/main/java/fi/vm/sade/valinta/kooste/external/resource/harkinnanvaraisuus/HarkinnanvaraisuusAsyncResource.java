package fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus;

import fi.vm.sade.valinta.kooste.external.resource.harkinnanvaraisuus.dto.HakemuksenHarkinnanvaraisuus;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Oppija;

public interface HarkinnanvaraisuusAsyncResource {

  Boolean hasYksilollistettyMatAi(HakemuksenHarkinnanvaraisuus h, Oppija o);
}
