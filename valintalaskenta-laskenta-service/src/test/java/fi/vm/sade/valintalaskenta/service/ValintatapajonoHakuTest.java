package fi.vm.sade.valintalaskenta.service;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.resource.ValintatapajonoResource;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Luo ja versioi valintatapajonoja. Testaa että uusimman version
 *         järjesteyskriteeritulokset palautuu valintatapajonohaulla
 * 
 */
@ContextConfiguration(locations = "classpath:test-context.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class ValintatapajonoHakuTest {

    @Autowired
    private ValintalaskentaService valintalaskentaService;

    @Autowired
    private ValintatapajonoResource valintatapajonoResource;

    @Test
    public void testVersioiValintatapajonojaJaHaeOikeatJarjestyskriteeritulokset() {

    }

}
