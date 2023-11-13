package fi.vm.sade.valintalaskenta.tulos.service.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.testdata.TestEntityDataUtil;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import java.util.*;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class TulosValintakoeOsallistuminenDAOTest extends AbstractIntegrationTest {

  @Autowired private TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO;

  @Autowired private ValintalaskentaModelMapper modelMapper;

  @Test
  public void testFindByHakuAndOsallistuminen() {
    final String hakuOid = "hakuOid1";
    final Osallistuminen osallistuminen = Osallistuminen.VIRHE;

    final String hakemusOid = "hakemusOid1";
    valintakoeOsallistuminenRepository.save(
        TestEntityDataUtil.luoValintakoeOsallistuminen(
            hakuOid,
            "hakija1",
            hakemusOid,
            Set.of(
                TestEntityDataUtil.luoHakutoiveEntity(
                    "hakukohde1",
                    Set.of(
                        TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                            0,
                            "vaihe1",
                            List.of(
                                TestEntityDataUtil.luoValintakoeEntity(
                                    "vk1", "vk1", osallistuminen, false, null))),
                        TestEntityDataUtil.luoValintakoeValinnanvaiheEntity(
                            0,
                            "vaihe2",
                            List.of(
                                TestEntityDataUtil.luoValintakoeEntity(
                                    "vk2", "vk2", osallistuminen, false, null))))))));

    List<ValintakoeOsallistuminen> osallistumiset =
        tulosValintakoeOsallistuminenDAO.findByHakuAndOsallistuminen(hakuOid, osallistuminen);
    assertEquals(1, osallistumiset.size());
    ValintakoeOsallistuminen vko = osallistumiset.get(0);

    assertNotNull(modelMapper.map(vko, ValintakoeOsallistuminenDTO.class));

    assertEquals(vko.getHakemusOid(), hakemusOid);
    assertEquals(1, vko.getHakutoiveet().size());
    Hakutoive hakutoive = vko.getHakutoiveetAsList().get(0);
    assertEquals(2, hakutoive.getValintakoeValinnanvaiheet().size());
    ValintakoeValinnanvaihe vaihe = hakutoive.getValintakoeValinnanvaiheetAsList().get(0);
    assertEquals(1, vaihe.getValintakokeet().size());
    Valintakoe koe = vaihe.getValintakokeetAsList().get(0);
    assertEquals(koe.getOsallistuminen(), osallistuminen);
  }
}
