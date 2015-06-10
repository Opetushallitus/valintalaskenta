package fi.vm.sade.valintalaskenta.tulos.service;

import fi.vm.sade.valintalaskenta.domain.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface ValintalaskentaTulosService {

    HakemusDTO haeTuloksetHakemukselle(String hakuoid, String hakemusoid);

    List<ValintatietoValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid);

    Optional<HakukohdeDTO> haeValinnanvaiheetHakukohteelleJaJonolle(String hakukohdeoid, List<String> valintatapajonot);

    List<HakijaryhmaDTO> haeHakijaryhmatHakukohteelle(String hakukohdeoid);

    List<HakukohdeDTO> haeVirheetHaulle(String hakuOid);

    List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid);

    MuokattuJonosija muutaJarjestyskriteeri(String valintatapajonoOid,
                                            String hakemusOid, Integer jarjestyskriteeriPrioriteetti,
                                            MuokattuJonosijaArvoDTO arvo);

    MuokattuJonosija poistaMuokattuJonosija(String valintatapajonoOid,
                                            String hakemusOid,
                                            Integer jarjestyskriteeriPrioriteetti);

    ValintakoeOsallistuminen haeValintakoeOsallistumiset(String hakemusOid);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset(Collection<String> hakemusOid);

    List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid);

    List<Jarjestyskriteerihistoria> haeJonosijaHistoria(String valintatapajonoOid, String hakemusOid);

    List<ValintakoeOsallistuminenDTO> haeValintakoevirheetHaulle(String hakuOid);

    void asetaHarkinnanvaraisestiHyvaksymisenTila(String hakuoid,
                                                  String hakukohdeoid, String hakemusoid,
                                                  HarkinnanvaraisuusTila hyvaksyttyHarkinannvaraisesti);

    List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisestiHyvaksymisenTila(String hakukohdeoid);

    List<HarkinnanvarainenHyvaksyminen> haeHakemuksenHarkinnanvaraisestiHyvaksymisenTilat(String hakuOid, String hakemusOid);

    ValinnanvaiheDTO lisaaTuloksia(ValinnanvaiheDTO vaihe, String hakukohdeoid, String tarjoajaOid);

    Optional<Valintatapajono> muokkaaSijotteluStatusta(String valintatapajonoOid, boolean status);

    boolean haeSijoitteluStatus(String valintatapajonoOid);
}
