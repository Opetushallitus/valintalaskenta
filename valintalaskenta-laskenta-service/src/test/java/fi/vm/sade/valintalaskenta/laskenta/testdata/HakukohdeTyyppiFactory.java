package fi.vm.sade.valintalaskenta.laskenta.testdata;

import java.util.UUID;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.stereotype.Component;

import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Tehdas hakukohdetyyppien luontiin - arvoilla ei ole väliä koska
 *         laskennan testaus kuuluu laskenta(valintaperusteet-laskenta) moduulin
 *         piiriin. Tehtaan avulla testiolioita voi nykiä suoraan kontekstista.
 */
@Component
public class HakukohdeTyyppiFactory implements FactoryBean<HakukohdeTyyppi> {

    public HakukohdeTyyppi getObject() throws Exception {
        HakukohdeTyyppi hakukohdeTyyppi = new HakukohdeTyyppi();
        hakukohdeTyyppi.setHakukohdeOid(UUID.randomUUID().toString());
        hakukohdeTyyppi.setPrioriteetti(10);
        return hakukohdeTyyppi;
    }

    public Class<?> getObjectType() {
        return HakukohdeTyyppi.class;
    }

    public boolean isSingleton() {
        return false;
    }
}
