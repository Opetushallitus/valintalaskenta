package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakuDTO;
import org.springframework.security.access.prepost.PreAuthorize;

import java.util.List;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

/**
 * Created by jukais on 24.3.2014.
 */
public interface ValintatietoService {

    //@PreAuthorize(READ_UPDATE_CRUD)
    List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(List<String> valintakoeOid,
                                                                 String hakukohdeOid);

    //@PreAuthorize(READ_UPDATE_CRUD)
    HakuDTO haeValintatiedot(String hakuOid);
}
