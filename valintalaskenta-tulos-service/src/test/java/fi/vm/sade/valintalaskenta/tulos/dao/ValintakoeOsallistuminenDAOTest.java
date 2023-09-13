package fi.vm.sade.valintalaskenta.tulos.dao;

import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import com.github.npathai.hamcrestopt.OptionalMatchers;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/** User: wuoti Date: 28.8.2013 Time: 15.47 */
@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class ValintakoeOsallistuminenDAOTest {

  @Autowired private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

  @Autowired private ApplicationContext applicationContext;

  @Autowired private ValintalaskentaModelMapper modelMapper;

  @Test
  public void testFindByHakuAndOsallistuminen() {
    final String hakuOid = "hakuOid1";
    final Osallistuminen osallistuminen = Osallistuminen.VIRHE;

    final String hakemusOid = "hakemusOid1";
    List<ValintakoeOsallistuminen> osallistumiset =
        valintakoeOsallistuminenDAO.findByHakuAndOsallistuminen(hakuOid, osallistuminen);
    assertEquals(1, osallistumiset.size());
    ValintakoeOsallistuminen vko = osallistumiset.get(0);

    ValintakoeOsallistuminenDTO dto = modelMapper.map(vko, ValintakoeOsallistuminenDTO.class);

    assertEquals(vko.getHakemusOid(), hakemusOid);
    assertEquals(1, vko.getHakutoiveet().size());
    Hakutoive hakutoive = vko.getHakutoiveet().get(0);
    assertEquals(2, hakutoive.getValintakoeValinnanvaiheet().size());
    ValintakoeValinnanvaihe vaihe = hakutoive.getValintakoeValinnanvaiheet().get(0);
    assertEquals(1, vaihe.getValintakokeet().size());
    Valintakoe koe = vaihe.getValintakokeet().get(0);
    assertEquals(koe.getOsallistuminen(), osallistuminen);
  }

  @Test
  public void testFindAmmatillisenKielikoeOsallistumiset() {
    List<ValintakoeOsallistuminen> osallistumiset =
        valintakoeOsallistuminenDAO.findAmmatillisenKielikoeOsallistumiset(
            date(LocalDate.of(2010, 1, 1)));
    assertEquals(1, osallistumiset.size());
    ValintakoeOsallistuminen vko = osallistumiset.get(0);

    ValintakoeOsallistuminenDTO dto = modelMapper.map(vko, ValintakoeOsallistuminenDTO.class);

    assertEquals("Timo-Testi", dto.getEtunimi());
    assertEquals("Rantalaiho-Testi", dto.getSukunimi());
    assertEquals("1.2.246.562.11.00000003337", dto.getHakemusOid());
    assertThat(vko.getHakutoiveet(), hasSize(4));

    Optional<Valintakoe> kielikoeOpt =
        vko.getHakutoiveet().stream()
            .flatMap(
                h ->
                    h.getValintakoeValinnanvaiheet().get(0).getValintakokeet().stream()
                        .filter(koe -> "kielikoe_fi".equals(koe.getValintakoeTunniste())))
            .findFirst();
    assertThat(kielikoeOpt, OptionalMatchers.isPresent());
    Valintakoe kielikoe = kielikoeOpt.get();

    assertEquals("kielikoe_fi", kielikoe.getValintakoeTunniste());
    assertEquals(
        Osallistuminen.EI_OSALLISTU, kielikoe.getOsallistuminen());
    assertEquals(true, kielikoe.getLaskentaTulos());

    assertThat(
        valintakoeOsallistuminenDAO.findAmmatillisenKielikoeOsallistumiset(
            date(LocalDate.of(2020, 1, 1))),
        hasSize(0));
  }

  private Date date(LocalDate from) {
    return Date.from(from.atTime(0, 0).toInstant(ZoneOffset.UTC));
  }
}
