package fi.vm.sade.valintalaskenta.tulos.service;

import java.util.List;

import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;
import fi.vm.sade.valintalaskenta.domain.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;

public interface ValintalaskentaTulosService {

    HakemusDTO haeTuloksetHakemukselle(String hakuoid, String hakemusoid);

    List<ValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid);

    List<Valintatapajono> haeVirheetHakukohteelle(String hakukohdeoid);

    List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid);

    MuokattuJonosija muutaJarjestyskriteeri(String valintatapajonoOid, String hakemusOid,
            Integer jarjestyskriteeriPrioriteetti, MuokattuJonosijaDTO arvo, String selite);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset(String hakijaoid);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid);

    List<JonosijaHistoria> haeJonosijaHistoria(String valintatapajonoOid, String hakemusOid);

}
