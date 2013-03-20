package fi.vm.sade.valintalaskenta.laskenta.testdata;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.stereotype.Component;

import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsuTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteviiteTyyppi;

@Component
public class JarjestyskriteeriTyyppiFactory implements FactoryBean<JarjestyskriteeriTyyppi> {

    public JarjestyskriteeriTyyppi getObject() throws Exception {
        JarjestyskriteeriTyyppi jarjestyskriteerityyppi = new JarjestyskriteeriTyyppi();
        FunktiokutsuTyyppi funktiokutsutyyppi = new FunktiokutsuTyyppi();
        funktiokutsutyyppi.setFunktionimi("HAELUKUARVO");
        ValintaperusteviiteTyyppi viite1 = new ValintaperusteviiteTyyppi();
        viite1.setOnPakollinen(true);
        viite1.setTunniste("Matematiikka");
        funktiokutsutyyppi.setValintaperusteviite(viite1);
        jarjestyskriteerityyppi.setFunktiokutsu(funktiokutsutyyppi);
        jarjestyskriteerityyppi.setPrioriteetti(1);
        return jarjestyskriteerityyppi;
    }

    public Class<?> getObjectType() {
        return JarjestyskriteeriTyyppi.class;
    }

    public boolean isSingleton() {
        return false;
    }
}
