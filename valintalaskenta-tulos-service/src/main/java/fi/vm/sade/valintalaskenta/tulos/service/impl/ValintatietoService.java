package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakuDTO;

import java.util.List;

/**
 * Created by jukais on 24.3.2014.
 */
public interface ValintatietoService {

    List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(List<String> valintakoeOid,
                                                                 String hakukohdeOid);

    HakuDTO haeValintatiedot(String hakuOid);
}
