package fi.vm.sade.valintalaskenta.tulos.service;

import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;
import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvaraisuusTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;

import java.util.List;

public interface ValintalaskentaTulosService {

    HakemusDTO haeTuloksetHakemukselle(String hakuoid, String hakemusoid);

    List<ValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid);

    List<HakukohdeDTO> haeVirheetHaulle(String hakuOid);

    List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid);

    MuokattuJonosija muutaJarjestyskriteeri(String valintatapajonoOid, String hakemusOid,
                                            Integer jarjestyskriteeriPrioriteetti, MuokattuJonosijaArvoDTO arvo, String selite);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset(String hakemusOid);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid);

    List<Jarjestyskriteerihistoria> haeJonosijaHistoria(String valintatapajonoOid, String hakemusOid);

    List<ValintakoeOsallistuminenDTO> haeValintakoevirheetHaulle(String hakuOid);

    void asetaHarkinnanvaraisestiHyvaksymisenTila(String hakuoid, String hakukohdeoid, String hakemusoid, HarkinnanvaraisuusTila hyvaksyttyHarkinannvaraisesti);

    List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisestiHyvaksymisenTila(String hakukohdeoid);
}
