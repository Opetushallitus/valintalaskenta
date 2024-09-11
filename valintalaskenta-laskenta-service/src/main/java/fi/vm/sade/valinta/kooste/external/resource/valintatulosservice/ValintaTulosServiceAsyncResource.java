package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice;

import fi.vm.sade.sijoittelu.domain.Valintatulos;
import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.raportointi.HakijaDTO;
import fi.vm.sade.valinta.kooste.external.resource.sijoittelu.ValintatulosUpdateStatus;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.AuditSession;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.Lukuvuosimaksu;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.LukuvuosimaksuMuutos;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.Valinnantulos;
import fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice.TilaHakijalleDto;
import fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice.VastaanottoAikarajaMennytDTO;
import io.reactivex.Observable;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public interface ValintaTulosServiceAsyncResource {
  DateTimeFormatter valintaTulosServiceCompatibleFormatter =
      DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss'Z'").withZoneUTC();

  Observable<String> getHakemuksenValintatulosAsString(String hakuOid, String hakemusOid);

  Observable<List<Valintatulos>> findValintatulokset(String hakuOid, String hakukohdeOid);

  Observable<List<Lukuvuosimaksu>> fetchLukuvuosimaksut(String hakukohdeOid, AuditSession session);

  Observable<String> saveLukuvuosimaksut(
      String hakukohdeOid, AuditSession session, List<LukuvuosimaksuMuutos> muutokset);

  Observable<List<Valintatulos>> findValintatuloksetIlmanHakijanTilaa(
      String hakuOid, String hakukohdeOid);

  Observable<List<Valintatulos>> findValintatuloksetByHakemus(String hakuOid, String hakemusOid);

  Observable<List<VastaanottoAikarajaMennytDTO>> findVastaanottoAikarajaMennyt(
      String hakuOid, String hakukohdeOid, Set<String> hakemusOids);

  Observable<List<TilaHakijalleDto>> findTilahakijalle(
      String hakuOid, String hakukohdeOid, String valintatapajonoOid, Set<String> hakemusOids);

  Observable<List<ValintatulosUpdateStatus>> postErillishaunValinnantulokset(
      AuditSession auditSession, String valintatapajonoOid, List<Valinnantulos> valinnantulokset);

  Observable<List<Valinnantulos>> getErillishaunValinnantulokset(
      AuditSession auditSession, String valintatapajonoOid);

  Observable<HakukohdeDTO> getHakukohdeBySijoitteluajoPlainDTO(String hakuOid, String hakukohdeOid);

  CompletableFuture<List<HakijaDTO>> getKoulutuspaikalliset(String hakuOid, String hakukohdeOid);

  CompletableFuture<List<HakijaDTO>> getKoulutuspaikalliset(String hakuOid);

  CompletableFuture<HakijaDTO> getHakijaByHakemus(String hakuOid, String hakemusOid);

  CompletableFuture<List<HakijaDTO>> getKaikkiHakijat(String hakuOid, String hakukohdeOid);

  CompletableFuture<List<HakijaDTO>> getHakijatIlmanKoulutuspaikkaa(String hakuOid);
}
