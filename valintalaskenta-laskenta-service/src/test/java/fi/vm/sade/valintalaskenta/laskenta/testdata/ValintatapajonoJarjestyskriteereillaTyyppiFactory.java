package fi.vm.sade.valintalaskenta.laskenta.testdata;

import java.util.UUID;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.stereotype.Component;

import fi.vm.sade.service.valintaperusteet.schema.TasasijasaantoTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component
public class ValintatapajonoJarjestyskriteereillaTyyppiFactory implements
        FactoryBean<ValintatapajonoJarjestyskriteereillaTyyppi> {

    public ValintatapajonoJarjestyskriteereillaTyyppi getObject() throws Exception {
        ValintatapajonoJarjestyskriteereillaTyyppi jono = new ValintatapajonoJarjestyskriteereillaTyyppi();
        jono.setAloituspaikat(5);
        jono.setKuvaus("Kuvaus");
        jono.setNimi("Nimi");
        jono.setOid(UUID.randomUUID().toString());
        jono.setPrioriteetti(1);
        jono.setSiirretaanSijoitteluun(true);
        jono.setTasasijasaanto(TasasijasaantoTyyppi.ALITAYTTO);
        // jono.getJarjestyskriteerit();
        return jono;
    }

    public Class<?> getObjectType() {
        return ValintatapajonoJarjestyskriteereillaTyyppi.class;
    }

    public boolean isSingleton() {
        return false;
    }

}
