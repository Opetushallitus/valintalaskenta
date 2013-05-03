package fi.vm.sade.valintalaskenta.tulos.service;

import fi.vm.sade.valintalaskenta.domain.*;

import java.util.List;

public interface ValintalaskentaTulosService {

    List<Versioituhakukohde> haeHakukohteet();

    List<Versioituhakukohde> haeHakukohteetHaulle(String hakuoid);

    List<Valinnanvaihe> haeValinnanvaiheetHakukohteelle(String hakukohdeoid);

    List<Valintatapajono> haeValintatapajonoValinnanvaiheelle(String valinnanvaiheoid);

    List<Jarjestyskriteeritulos> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid);

    List<Hakukohde> haeLasketutValinnanvaiheetHaulle(String hakuOid);
}
