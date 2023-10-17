package fi.vm.sade.valintalaskenta.tulos.service.dao;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.*;
import fi.vm.sade.valintalaskenta.laskenta.testdata.TestDataUtil;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class TulosValintakoeOsallistuminenDAOTest extends AbstractIntegrationTest {

  @Autowired
  private TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO;

  @Autowired private ValintalaskentaModelMapper modelMapper;

  @Test
  public void testFindByHakuAndOsallistuminen() {
    final String hakuOid = "hakuOid1";
    final Osallistuminen osallistuminen = Osallistuminen.VIRHE;

    final String hakemusOid = "hakemusOid1";
    valintakoeOsallistuminenRepository.save(TestDataUtil.luoValintakoeOsallistuminen(hakuOid, "hakija1", hakemusOid,
      Set.of(TestDataUtil.luoHakutoiveEntity("hakukohde1", Set.of(
        TestDataUtil.luoValintakoeValinnanvaiheEntity(0, "vaihe1", List.of(
          TestDataUtil.luoValintakoeEntity("vk1", "vk1", osallistuminen, false, null))),
        TestDataUtil.luoValintakoeValinnanvaiheEntity(0, "vaihe2", List.of(
          TestDataUtil.luoValintakoeEntity("vk2", "vk2", osallistuminen, false, null)))
        )))));


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
