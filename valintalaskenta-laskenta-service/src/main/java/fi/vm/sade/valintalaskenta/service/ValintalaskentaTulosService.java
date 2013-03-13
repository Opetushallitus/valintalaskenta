package fi.vm.sade.valintalaskenta.service;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;

import java.util.List;

public interface ValintalaskentaTulosService {

    List<Versioituhakukohde> haeHakukohteet();

    List<Versioituhakukohde> haeHakukohteetHaulle(String hakuoid);

    List<Valinnanvaihe> haeValinnanvaiheetHakukohteelle(String hakukohdeoid);

    List<Valintatapajono> haeValintatapajonoValinnanvaiheelle(String valinnanvaiheoid);

    List<Jarjestyskriteeritulos> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid);
}
