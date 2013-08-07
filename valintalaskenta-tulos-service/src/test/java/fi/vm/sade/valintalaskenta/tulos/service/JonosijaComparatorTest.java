package fi.vm.sade.valintalaskenta.tulos.service;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

/**
 * Created with IntelliJ IDEA. User: kkammone
 * 
 * Date: 7.6.2013 Time: 9:18 To change this template use File | Settings | File
 * Templates.
 */

@ContextConfiguration(locations = "classpath:application-context-test.xml")
@RunWith(org.springframework.test.context.junit4.SpringJUnit4ClassRunner.class)
@UsingDataSet
public class JonosijaComparatorTest {

    @Autowired
    private ValintatulosConverter valintatulosConverter;

    @Test
    public void testCompartor() {

        java.util.List<fi.vm.sade.valintalaskenta.domain.Jonosija> list = new java.util.ArrayList<fi.vm.sade.valintalaskenta.domain.Jonosija>();

        fi.vm.sade.valintalaskenta.domain.Jonosija sija0 = new fi.vm.sade.valintalaskenta.domain.Jonosija();
        sija0.setHakemusoid("sija0");
        fi.vm.sade.valintalaskenta.domain.Jonosija sija1 = new fi.vm.sade.valintalaskenta.domain.Jonosija();
        sija1.setHakemusoid("sija1");
        fi.vm.sade.valintalaskenta.domain.Jonosija sija2 = new fi.vm.sade.valintalaskenta.domain.Jonosija();
        sija2.setHakemusoid("sija2");
        fi.vm.sade.valintalaskenta.domain.Jonosija sija3 = new fi.vm.sade.valintalaskenta.domain.Jonosija();
        sija3.setHakemusoid("sija3");
        fi.vm.sade.valintalaskenta.domain.Jonosija sija4 = new fi.vm.sade.valintalaskenta.domain.Jonosija();
        sija4.setHakemusoid("sija4");



        list.add(sija1);
        list.add(sija0);
        list.add(sija3);
        list.add(sija2);
        list.add(sija4);


        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s0t1 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s0t2 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        s0t1.setArvo(new java.math.BigDecimal("2"));
        s0t2.setArvo(new java.math.BigDecimal("2"));
        sija1.getJarjestyskriteerit().put(1, s0t1);
        sija1.getJarjestyskriteerit().put(2, s0t2);
        sija0.setHarkinnanvarainen(true);
        s0t1.setTila(fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI);


        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s1t1 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s1t2 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        s1t1.setArvo(new java.math.BigDecimal("2"));
        s1t2.setArvo(new java.math.BigDecimal("2"));
        sija1.getJarjestyskriteerit().put(1, s1t1);
        sija1.getJarjestyskriteerit().put(2, s1t2);

        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s2t1 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s2t2 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        s2t1.setArvo(new java.math.BigDecimal("2"));
        s2t2.setArvo(new java.math.BigDecimal("2"));
        sija2.getJarjestyskriteerit().put(1, s2t1);
        sija2.getJarjestyskriteerit().put(2, s2t2);

        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s3t1 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s3t2 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        s3t1.setArvo(new java.math.BigDecimal("4"));
        s3t2.setArvo(new java.math.BigDecimal("1"));
        sija3.getJarjestyskriteerit().put(1, s3t1);
        sija3.getJarjestyskriteerit().put(2, s3t2);

        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s4t1 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos s4t2 = new fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos();
        s4t1.setArvo(new java.math.BigDecimal("3"));
        s4t2.setArvo(new java.math.BigDecimal("1"));
        sija4.getJarjestyskriteerit().put(1, s4t1);
        sija4.getJarjestyskriteerit().put(2, s4t2);

        for (fi.vm.sade.valintalaskenta.domain.Jonosija j : list) {
            System.out.println("Before sort: " + j.getHakemusoid());
        }


        java.util.List<JonosijaDTO> a = valintatulosConverter.jarjestaJaLisaaJonosijaNumero(list);

        for (fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO j : a) {
            System.out.println("After sort: " + j.getHakemusOid());
        }

        junit.framework.Assert.assertEquals(a.get(1).getHakemusOid(), sija3.getHakemusoid());
        junit.framework.Assert.assertEquals(a.get(2).getHakemusOid(), sija4.getHakemusoid());

    }

}
