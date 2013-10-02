package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;

import java.util.List;

public interface ValintakoeOsallistuminenDAO {

    //  List<ValintakoeOsallistuminen> findAll();

    List<ValintakoeOsallistuminen> findByHakemusOid(String hakijaoid);

    List<ValintakoeOsallistuminen> findByHakutoive(String hakukohdeOid);

    List<ValintakoeOsallistuminen> findByHakuAndOsallistuminen(String hakuOid, Osallistuminen osallistuminen);
}
