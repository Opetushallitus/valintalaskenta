package fi.vm.sade.valintalaskenta.tulos.service;

import java.math.BigDecimal;
import java.util.List;

import fi.vm.sade.valintalaskenta.domain.*;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;

public interface ValintalaskentaTulosService {


 //   List<Versioituhakukohde> haeHakukohteet();
 //  List<Versioituhakukohde> haeHakukohteetHaulle(String hakuoid);
 //  List<Valintatapajono> haeValintatapajonoValinnanvaiheelle(String valinnanvaiheoid);
 //  List<Jonosija> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid);


    List<ValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid);

    List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid);



    MuokattuJonosija muutaJarjestyskriteerinArvo(String valintatapajonoOid, String hakemusOid,
                                                 Integer jarjestyskriteeriPrioriteetti, BigDecimal arvo, String selite);

    MuokattuJonosija muutaJarjestyskriteerinTila(String valintatapajonoOid, String hakemusOid, Integer jarjestyskriteeriPrioriteetti, JarjestyskriteerituloksenTila arvo, String selite);



    List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset(String hakijaoid);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid);

    List<JonosijaHistoria> haeJonosijaHistoria(String valintatapajonoOid, String hakemusOid);

    //  List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset();

}
