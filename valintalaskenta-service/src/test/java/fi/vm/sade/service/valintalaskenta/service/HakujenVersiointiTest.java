package fi.vm.sade.service.valintalaskenta.service;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import fi.vm.sade.service.valintalaskenta.ValintalaskentaService;
import fi.vm.sade.service.valintalaskenta.resource.HakukohdeResource;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Tekee laskennan muutamia kertoja samalle Valinnanvaiheelle ja
 *         verifioi että REST-kysely palauttaa uusimman version
 */
@ContextConfiguration(locations = "classpath:test-context.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class HakujenVersiointiTest {

    private static final Logger LOG = LoggerFactory.getLogger(HakujenVersiointiTest.class);

    @Autowired
    private ValintalaskentaService valintalaskentaService;

    @Autowired
    private HakukohdeResource hakukohdeResource;

    @SuppressWarnings("unchecked")
    @Test
    public void testVersionti() {

    }
}
