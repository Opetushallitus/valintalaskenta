package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import java.util.List;

public interface TulosValintakoeOsallistuminenRepositoryCustom {

  List<ValintakoeOsallistuminen> findDistinctValintakoeOsallistuminensByHakutoive(
      String hakukohdeOid);

  List<ValintakoeOsallistuminen> findDistinctValintakoeOsallistuminensByHakutoiveet(
      List<String> hakukohdeOids);

  List<ValintakoeOsallistuminen> findDistinctByHakuAndOsallistuminen(
      String hakuOid, Osallistuminen osallistuminen);
}
