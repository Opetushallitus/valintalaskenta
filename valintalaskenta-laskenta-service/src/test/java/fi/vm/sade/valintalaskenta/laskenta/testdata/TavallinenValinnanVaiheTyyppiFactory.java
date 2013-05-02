package fi.vm.sade.valintalaskenta.laskenta.testdata;

import fi.vm.sade.service.valintaperusteet.schema.TavallinenValinnanVaiheTyyppi;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.stereotype.Component;
import scala.actors.threadpool.AtomicInteger;

import java.util.UUID;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 12.00
 */
@Component
public class TavallinenValinnanVaiheTyyppiFactory implements FactoryBean<TavallinenValinnanVaiheTyyppi> {
    private static final AtomicInteger COUNTER = new AtomicInteger(0);


    @Override
    public TavallinenValinnanVaiheTyyppi getObject() throws Exception {
        TavallinenValinnanVaiheTyyppi vaihe = new TavallinenValinnanVaiheTyyppi();
        vaihe.setValinnanVaiheJarjestysluku(COUNTER.incrementAndGet());
        vaihe.setValinnanVaiheOid(UUID.randomUUID().toString());
        return vaihe;
    }

    @Override
    public Class<?> getObjectType() {
        return TavallinenValinnanVaiheTyyppi.class;
    }

    @Override
    public boolean isSingleton() {
        return false;
    }
}
