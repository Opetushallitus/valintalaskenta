package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import java.time.LocalDateTime;
import java.util.List;

public interface TulosValintakoeOsallistuminenDAO {
  ValintakoeOsallistuminen findByHakemusOid(String hakemusOid);

  List<ValintakoeOsallistuminen> findByHakijaOids(List<String> hakijaOids);

  List<ValintakoeOsallistuminen> findByHakutoive(String hakukohdeOid);

  List<ValintakoeOsallistuminen> findByHakutoiveet(List<String> hakukohdeOids);

  List<ValintakoeOsallistuminen> findByHakuAndOsallistuminen(
      String hakuOid, Osallistuminen osallistuminen);

  List<String> readNewOrModifiedHakemusOids(LocalDateTime startDatetime, LocalDateTime endDatatime);
}
