package fi.vm.sade.valintalaskenta.laskenta.testdata;

import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.stereotype.Component;
import scala.actors.threadpool.AtomicInteger;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Tehdas valintaperusteettyyppien luontiin - arvoilla ei ole väliä
 *         koska laskennan testaus kuuluu laskenta(valintaperusteet-laskenta)
 *         moduulin piiriin. Tehtaan avulla testiolioita voi nykiä suoraan
 *         kontekstista.
 */
@Component
public class ValintaperusteetTyyppiFactory implements FactoryBean<ValintaperusteetTyyppi> {

    private static final AtomicInteger COUNTER = new AtomicInteger(0);

    public ValintaperusteetTyyppi getObject() throws Exception {
        ValintaperusteetTyyppi valintaperusteettyyppi = new ValintaperusteetTyyppi();
        valintaperusteettyyppi.setHakukohdeOid(null);
        return valintaperusteettyyppi;
    }

    public Class<?> getObjectType() {
        return ValintaperusteetTyyppi.class;
    }

    public boolean isSingleton() {
        return false;
    }

}
