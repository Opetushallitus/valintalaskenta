package fi.vm.sade.valintalaskenta.tulos.dao;

import com.google.gson.Gson;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import java.io.InputStreamReader;
import org.junit.Assert;
import org.junit.Test;

public class ValinnanvaiheMappingTest {

  @Test
  public void testaaMappaus() {
    ValintalaskentaModelMapper modelMapper = new ValintalaskentaModelMapper();

    ValinnanvaiheDTO v =
        new Gson()
            .fromJson(
                new InputStreamReader(
                    ValinnanvaiheMappingTest.class.getResourceAsStream("valinnanvaiheDTO.json")),
                ValinnanvaiheDTO.class);

    // Valinnanvaihe v0 = new Gson().fromJson(
    // new InputStreamReader(ValinnanvaiheMappingTest.class
    // .getResourceAsStream("valinnanvaihe.json")),
    // Valinnanvaihe.class);

    Valinnanvaihe v1 = modelMapper.map(v, Valinnanvaihe.class);
    // System.err.println(new GsonBuilder().setPrettyPrinting().create()
    // .toJson(v1));

    Assert.assertEquals("Oidit ei ole samoja!", v.getValinnanvaiheoid(), v1.getValinnanVaiheOid());
  }
}
