package fi.vm.sade.valintalaskenta.laskenta.testdata;

import java.util.UUID;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.stereotype.Component;

import scala.actors.threadpool.AtomicInteger;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Tehdas hakemustyyppien luontiin - arvoilla ei ole väliä koska
 *         laskennan testaus kuuluu laskenta(valintaperusteet-laskenta) moduulin
 *         piiriin. Tehtaan avulla testiolioita voi nykiä suoraan kontekstista.
 */
@Component
public class HakemusTyyppiFactory implements FactoryBean<HakemusTyyppi> {

    private static final String[] ETUNIMIA = { "Pentti", "Jorma", "Liisa", "Essi", "Mauri", "Hannu" };
    private static final String[] SUKUNIMIA = { "Lötjönen", "Kuustonen", "Blomqvist", "Keskimäki", "Laakso" };
    private static final AtomicInteger COUNTER = new AtomicInteger(0);

    private String getEtunimi() {
        return ETUNIMIA[COUNTER.incrementAndGet() % ETUNIMIA.length];
    }

    private String getSukunimi() {
        return SUKUNIMIA[COUNTER.incrementAndGet() % SUKUNIMIA.length];
    }

    public HakemusTyyppi getObject() throws Exception {
        HakemusTyyppi hakemusTyyppi = new HakemusTyyppi();
        hakemusTyyppi.setHakemusOid(UUID.randomUUID().toString());
        hakemusTyyppi.setHakijaOid(UUID.randomUUID().toString());
        hakemusTyyppi.setHakijanEtunimi(getEtunimi());
        hakemusTyyppi.setHakijanSukunimi(getSukunimi());
        // hakemusTyyppi.getHakutoive();
        return hakemusTyyppi;
    }

    public Class<?> getObjectType() {
        return HakemusTyyppi.class;
    }

    public boolean isSingleton() {
        return false;
    }

}
