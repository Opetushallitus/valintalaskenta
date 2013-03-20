package fi.vm.sade.valintalaskenta.laskenta.testdata;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.stereotype.Component;

import scala.actors.threadpool.AtomicInteger;
import fi.vm.sade.service.hakemus.schema.AvainArvoTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Tehdas avainarvotyyppien luontiin - arvoilla ei ole väliä koska
 *         laskennan testaus kuuluu laskenta(valintaperusteet-laskenta) moduulin
 *         piiriin. Tehtaan avulla testiolioita voi nykiä suoraan kontekstista.
 */
@Component
public class AvainArvoTyyppiFactory implements FactoryBean<AvainArvoTyyppi> {

    private static final String[] AVAIMIA = { "Äidinkieli", "Filosofia", "Matematiikka", "Psykologia", "Maantieto",
            "Uskonto" };
    private static final String[] ARVOJA = { "5.0", "9.0", "7.0", "10.0" };
    private static final AtomicInteger COUNTER0 = new AtomicInteger(0);
    private static final AtomicInteger COUNTER1 = new AtomicInteger(0);

    private String getAvain() {
        return AVAIMIA[COUNTER0.incrementAndGet() % AVAIMIA.length];
    }

    private String getArvo() {
        return ARVOJA[COUNTER1.incrementAndGet() % ARVOJA.length];
    }

    public AvainArvoTyyppi getObject() throws Exception {
        AvainArvoTyyppi avainArvoTyyppi = new AvainArvoTyyppi();
        avainArvoTyyppi.setArvo(getArvo());
        avainArvoTyyppi.setAvain(getAvain());
        return avainArvoTyyppi;
    }

    public Class<?> getObjectType() {
        return AvainArvoTyyppi.class;
    }

    public boolean isSingleton() {
        return false;
    }
}
