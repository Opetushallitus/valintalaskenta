package fi.vm.sade.valintalaskenta.tulos.service;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import scala.actors.threadpool.Arrays;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.service.impl.ExcelExportServiceImpl;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Savutesti Excel Exportille
 */
@ContextConfiguration(classes = ExcelExportServiceTest.class)
// locations = "classpath:application-context-test.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class ExcelExportServiceTest {

    private final String VALID_HAKUKOHDEOID = "VALID_HAKUKOHDEOID";
    private final String INVALID_HAKUKOHDEOID = "INVALID_HAKUKOHDEOID";

    @Bean
    public ValintalaskentaTulosService createValintalaskentaTulosService() {
        ValintalaskentaTulosService ts = mock(ValintalaskentaTulosService.class);
        Valinnanvaihe v = new Valinnanvaihe();
        Valintatapajono jono = new Valintatapajono();

        v.getValintatapajono().add(jono);
        List<Valinnanvaihe> full = Arrays.asList(new Valinnanvaihe[] { v });
        when(ts.haeValinnanvaiheetHakukohteelle(VALID_HAKUKOHDEOID)).thenReturn(full);
        List<Valinnanvaihe> empty = Collections.emptyList();
        when(ts.haeValinnanvaiheetHakukohteelle(INVALID_HAKUKOHDEOID)).thenReturn(empty);
        return ts;
    }

    @Bean
    public ExcelExportService createExcelExportService() {
        return new ExcelExportServiceImpl();
    }

    @Autowired
    ExcelExportService exportService;

    @Test
    public void exportValintatulokset() {

        Assert.assertFalse(
                "Valid Excel export should not equal invalid!",
                exportService.exportValintalaskentatulos(VALID_HAKUKOHDEOID).equals(
                        exportService.exportValintalaskentatulos(INVALID_HAKUKOHDEOID)));

    }
}
