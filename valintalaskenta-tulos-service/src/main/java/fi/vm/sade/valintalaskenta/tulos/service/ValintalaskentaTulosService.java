package fi.vm.sade.valintalaskenta.tulos.service;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;
import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvaraisuusTila;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

public interface ValintalaskentaTulosService {

  List<JonoDto> haeJonotSijoittelussa(String hakuOid);

  HakemusDTO haeTuloksetHakemukselle(String hakuoid, String hakemusoid);

  List<ValintatietoValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid);

  Optional<HakukohdeDTO> haeValinnanvaiheetHakukohteelleJaJonolle(
      String hakukohdeoid, List<String> valintatapajonot);

  List<HakijaryhmaDTO> haeHakijaryhmatHakukohteelle(String hakukohdeoid);

  List<HakukohdeDTO> haeVirheetHaulle(String hakuOid);

  List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid);

  Stream<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(
      String hakuOid, Function<HakukohdeDTO, HakukohdeDTO> convert);

  MuokattuJonosija muutaJarjestyskriteeri(
      String valintatapajonoOid,
      String hakemusOid,
      Integer jarjestyskriteeriPrioriteetti,
      MuokattuJonosijaArvoDTO arvo,
      User auditUser);

  MuokattuJonosija poistaMuokattuJonosija(
      String valintatapajonoOid,
      String hakemusOid,
      Integer jarjestyskriteeriPrioriteetti,
      User auditUser);

  ValintakoeOsallistuminen haeValintakoeOsallistumiset(String hakemusOid);

  List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid);

  List<ValintakoeOsallistuminenDTO> haeValintakoevirheetHaulle(String hakuOid);

  void asetaHarkinnanvaraisestiHyvaksymisenTila(
      String hakuoid,
      String hakukohdeoid,
      String hakemusoid,
      HarkinnanvaraisuusTila hyvaksyttyHarkinannvaraisesti,
      User auditUser);

  List<HarkinnanvarainenHyvaksyminen> haeHarkinnanvaraisestiHyvaksymisenTila(String hakukohdeoid);

  List<HarkinnanvarainenHyvaksyminen> haeHakemuksenHarkinnanvaraisestiHyvaksymisenTilat(
      String hakuOid, String hakemusOid);

  ValinnanvaiheDTO lisaaTuloksia(ValinnanvaiheDTO vaihe, String hakukohdeoid, String tarjoajaOid);

  Optional<Valintatapajono> muokkaaValintatapajonoa(
      String valintatapajonoOid, Consumer<Valintatapajono> muokkausFunktio, User auditUser);

  boolean haeSijoitteluStatus(String valintatapajonoOid);

  List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakukohdes(
      List<String> hakukohdeOids);

  List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakijas(List<String> hakijaOids);
}
