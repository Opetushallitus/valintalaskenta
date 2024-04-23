package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu.SijoitteluJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu.SijoitteluValintatapajono;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

public interface TulosValinnanvaiheRepository extends CrudRepository<Valinnanvaihe, UUID> {

  @Query(
      value =
          "select distinct vv.* from Valinnanvaihe vv join Valintatapajono vtpjono on vtpjono.valinnanvaihe = vv.id join Jonosija js on js.valintatapajono = vtpjono.id where vv.hakukohde_oid = :hakukohdeOid")
  List<Valinnanvaihe> findDistinctValinnanvaihesByHakukohdeOid(
      @Param("hakukohdeOid") String hakukohdeOid);

  Stream<Valinnanvaihe> findDistinctValinnanvaihesByHakuOid(String hakuOid);

  @Query("select distinct vv.* from Valinnanvaihe vv where vv.haku_oid = :hakuOid")
  List<Valinnanvaihe> findDistinctValinnanvaihesByHakuOidAsList(@Param("hakuOid") String hakuOid);

  @Query(
      "select distinct vv.* from Valinnanvaihe vv join Valintatapajono vtpjono on vtpjono.valinnanvaihe = vv.id join Jonosija js on js.valintatapajono = vtpjono.id where js.hakemus_oid = :hakemusOid and vv.haku_oid = :hakuOid")
  List<Valinnanvaihe> findDistinctValinnanvaihesByHakuOidAndHakemusOid(
      @Param("hakuOid") String hakuOid, @Param("hakemusOid") String hakemusOid);

  @Query(
      "select * from Valinnanvaihe vv join Valintatapajono vtpjono on vtpjono.valinnanvaihe = vv.id where vtpjono.valintatapajono_oid = :valintatapajonoOid")
  Optional<Valinnanvaihe> findValinnanvaiheByValintatapajono(
      @Param("valintatapajonoOid") String valintatapajonoOid);

  Optional<Valinnanvaihe> findValinnanvaiheByValinnanvaiheOid(String valinnanvaiheOid);

  @Query(
      "select js.id as jonosija_id, js.hakemus_oid, js.hakija_oid, js.syotetyt_arvot, js.jarjestyskriteeritulokset, "
          + "js.hakutoiveprioriteetti, js.harkinnanvarainen, js.hylatty_valisijoittelussa, js.valintatapajono "
          + "from Jonosija js join Valintatapajono jono on jono.id = js.valintatapajono join Valinnanvaihe vv on vv.id = jono.valinnanvaihe "
          + "where vv.haku_oid = :hakuOid")
  List<SijoitteluJonosija> haeSijoittelunJonosijatJaJarjestyskriteerit(
      @Param("hakuOid") String hakuOid);

  @Query(
      "select jono.aloituspaikat, jono.ei_varasijatayttoa, jono.kaytetaan_valintalaskentaa, jono.kaikki_ehdon_tayttavat_hyvaksytaan, "
          + "jono.poissa_oleva_taytto, jono.valintatapajono_oid, jono.id, jono.prioriteetti, jono.siirretaan_sijoitteluun, jono.tasasijasaanto, jono.valmis_sijoiteltavaksi,"
          + "jono.sijoitteluajo_id, jono.kaytetaan_kokonaispisteita, jono.nimi, "
          + "vv.created_at, vv.valinnanvaihe_oid, vv.nimi as valinnanvaihe_nimi, vv.jarjestysnumero, vv.hakukohde_oid "
          + "from Valintatapajono jono join Valinnanvaihe vv on vv.id = jono.valinnanvaihe where vv.haku_oid = :hakuOid")
  List<SijoitteluValintatapajono> findValintatapajonoWithValinnanvaiheForSijoittelu(
      @Param("hakuOid") String hakuOid);

  @Query(
      "select hk_oid from ("
          + "select hakukohde_oid hk_oid from valinnanvaihe where last_modified between :start and :end"
          + " union "
          + "select vv.hakukohde_oid hk_oid from valinnanvaihe vv "
          + "join valintatapajono vtpj on vtpj.valinnanvaihe = vv.id where vtpj.last_modified between :start and :end"
          + " union "
          + "select vv.hakukohde_oid hk_oid from valinnanvaihe vv "
          + "join Valintatapajono vtpj on vtpj.valinnanvaihe = vv.id join jonosija js on js.valintatapajono = vtpj.id "
          + "where js.last_modified between :start and :end"
          + " union "
          + "select vv.hakukohde_oid hk_oid from valinnanvaihe vv "
          + "join muokattu_jonosija mjs on mjs.hakukohde_oid = vv.hakukohde_oid "
          + "where mjs.last_modified between :start and :end"
          + " union "
          + "select vv.hakukohde_oid hk_oid from valinnanvaihe vv "
          + "join harkinnanvarainen_hyvaksyminen hh on hh.hakukohde_oid = vv.hakukohde_oid "
          + "where hh.last_modified between :start and :end) hks")
  List<String> findHakukohdeOidsByTimeRange(
      @Param("start") LocalDateTime startDateTime, @Param("end") LocalDateTime endDatetime);

  @Query(
      "select hk_oid from ("
          + "select hakukohde_oid hk_oid from valinnanvaihe where last_modified < :end"
          + " union "
          + "select vv.hakukohde_oid hk_oid from valinnanvaihe vv "
          + "join valintatapajono vtpj on vtpj.valinnanvaihe = vv.id where vtpj.last_modified < :end"
          + " union "
          + "select vv.hakukohde_oid hk_oid from valinnanvaihe vv "
          + "join Valintatapajono vtpj on vtpj.valinnanvaihe = vv.id join jonosija js on js.valintatapajono = vtpj.id "
          + "where js.last_modified < :end"
          + " union "
          + "select vv.hakukohde_oid hk_oid from valinnanvaihe vv "
          + "join muokattu_jonosija mjs on mjs.hakukohde_oid = vv.hakukohde_oid where mjs.last_modified < :end"
          + " union "
          + "select vv.hakukohde_oid hk_oid from valinnanvaihe vv "
          + "join harkinnanvarainen_hyvaksyminen hh on hh.hakukohde_oid = vv.hakukohde_oid "
          + "where hh.last_modified < :end) hks")
  List<String> findHakukohdeOidsByEndTime(@Param("end") LocalDateTime endDatetime);
}
