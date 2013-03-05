package fi.vm.sade.valintalaskenta.service;

import java.util.List;

import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;

public interface ValintalaskentaTulosService {

    List<Hakukohde> haeHakukohteet();

    List<Hakukohde> haeHakukohteetHaulle(String hakuoid);

    List<Valinnanvaihe> haeValinnanvaiheetHakukohteelle(String hakukohdeoid);

    List<Valintatapajono> haeValintatapajonoValinnanvaiheelle(String valinnanvaiheoid);

    List<Jarjestyskriteeritulos> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid);
}
