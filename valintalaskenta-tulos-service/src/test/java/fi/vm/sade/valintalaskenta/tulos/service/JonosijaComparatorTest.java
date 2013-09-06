package fi.vm.sade.valintalaskenta.tulos.service;

import com.lordofthejars.nosqlunit.annotation.UsingDataSet;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import java.util.List;

/**
 * Created with IntelliJ IDEA. User: kkammone
 * <p/>
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
    public void testComparator() {

        java.util.List<Jonosija> list = new java.util.ArrayList<Jonosija>();

        Jonosija sija0 = new Jonosija();
        sija0.setHakemusOid("sija0");
        Jonosija sija1 = new Jonosija();
        sija1.setHakemusOid("sija1");
        Jonosija sija2 = new Jonosija();
        sija2.setHakemusOid("sija2");
        Jonosija sija3 = new Jonosija();
        sija3.setHakemusOid("sija3");
        Jonosija sija4 = new Jonosija();
        sija4.setHakemusOid("sija4");


        list.add(sija1);
        list.add(sija0);
        list.add(sija3);
        list.add(sija2);
        list.add(sija4);


        Jarjestyskriteeritulos s0t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s0t2 = new Jarjestyskriteeritulos();
        s0t1.setArvo(new java.math.BigDecimal("2"));
        s0t1.setPrioriteetti(1);
        s0t2.setArvo(new java.math.BigDecimal("2"));
        s0t2.setPrioriteetti(2);
        sija0.getJarjestyskriteeritulokset().add(s0t1);
        sija0.getJarjestyskriteeritulokset().add(s0t2);
        s0t1.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI);
        sija0.setHarkinnanvarainen(true);

        Jarjestyskriteeritulos s1t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s1t2 = new Jarjestyskriteeritulos();
        s1t1.setArvo(new java.math.BigDecimal("2"));
        s1t1.setPrioriteetti(1);
        s1t2.setArvo(new java.math.BigDecimal("2"));
        s1t2.setPrioriteetti(2);
        sija1.getJarjestyskriteeritulokset().add(s1t1);
        sija1.getJarjestyskriteeritulokset().add(s1t1);

        Jarjestyskriteeritulos s2t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s2t2 = new Jarjestyskriteeritulos();
        s2t1.setArvo(new java.math.BigDecimal("2"));
        s2t1.setPrioriteetti(1);
        s2t2.setArvo(new java.math.BigDecimal("2"));
        s2t2.setPrioriteetti(2);
        sija2.getJarjestyskriteeritulokset().add(s2t1);
        sija2.getJarjestyskriteeritulokset().add(s2t2);

        Jarjestyskriteeritulos s3t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s3t2 = new Jarjestyskriteeritulos();
        s3t1.setArvo(new java.math.BigDecimal("4"));
        s3t1.setPrioriteetti(1);
        s3t2.setArvo(new java.math.BigDecimal("1"));
        s3t2.setPrioriteetti(2);
        sija3.getJarjestyskriteeritulokset().add(s3t1);
        sija3.getJarjestyskriteeritulokset().add(s3t2);

        Jarjestyskriteeritulos s4t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s4t2 = new Jarjestyskriteeritulos();
        s4t1.setArvo(new java.math.BigDecimal("3"));
        s4t1.setPrioriteetti(1);
        s4t2.setArvo(new java.math.BigDecimal("1"));
        s4t2.setPrioriteetti(2);
        sija4.getJarjestyskriteeritulokset().add(s4t1);
        sija4.getJarjestyskriteeritulokset().add(s4t2);

        for (Jonosija j : list) {
            System.out.println("Before sort: " + j.getHakemusOid());
        }


        List<JonosijaDTO> a = valintatulosConverter.convertJonosija(list);
        valintatulosConverter.sort(a);

        for (JonosijaDTO j : a) {
            System.out.println("After sort: " + j.getHakemusOid());
        }

        junit.framework.Assert.assertEquals(a.get(0).getHakemusOid(), sija0.getHakemusOid());
        junit.framework.Assert.assertEquals(a.get(1).getHakemusOid(), sija3.getHakemusOid());
        junit.framework.Assert.assertEquals(a.get(2).getHakemusOid(), sija4.getHakemusOid());

    }

}
