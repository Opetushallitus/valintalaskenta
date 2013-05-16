package fi.vm.sade.valintalaskenta.tulos.service;

import fi.vm.sade.valintalaskenta.domain.*;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;

import java.util.List;

public interface ValintalaskentaTulosService {

    List<Versioituhakukohde> haeHakukohteet();

    List<Versioituhakukohde> haeHakukohteetHaulle(String hakuoid);

    List<Valinnanvaihe> haeValinnanvaiheetHakukohteelle(String hakukohdeoid);

    List<Valintatapajono> haeValintatapajonoValinnanvaiheelle(String valinnanvaiheoid);

    List<Jonosija> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset();

    List<Hakukohde> haeLasketutValinnanvaiheetHaulle(String hakuOid);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset(String hakijaoid);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid);

}
