package fi.vm.sade.service.valintaperusteet.algoritmi.domain.comparator;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.converter.JonosijaToJonosijaDTOConverter;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import junit.framework.Assert;

import org.junit.Test;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Jonosija;

/**
 * Created with IntelliJ IDEA. User: kkammone
 * 
 * Date: 7.6.2013 Time: 9:18 To change this template use File | Settings | File
 * Templates.
 */

public class JonosijaComparatorTest {

    @Test
    public void testCompartor() {

        List<Jonosija> list = new ArrayList<Jonosija>();

        Jonosija sija0 = new Jonosija();
        sija0.setHakemusoid("sija0");
        Jonosija sija1 = new Jonosija();
        sija1.setHakemusoid("sija1");
        Jonosija sija2 = new Jonosija();
        sija2.setHakemusoid("sija2");
        Jonosija sija3 = new Jonosija();
        sija3.setHakemusoid("sija3");
        Jonosija sija4 = new Jonosija();
        sija4.setHakemusoid("sija4");



        list.add(sija1);
        list.add(sija0);
        list.add(sija3);
        list.add(sija2);
        list.add(sija4);


        Jarjestyskriteeritulos s0t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s0t2 = new Jarjestyskriteeritulos();
        s0t1.setArvo(new BigDecimal("2"));
        s0t2.setArvo(new BigDecimal("2"));
        sija1.getJarjestyskriteerit().put(1, s0t1);
        sija1.getJarjestyskriteerit().put(2, s0t2);
        sija0.setHarkinnanvarainen(true);
        s0t1.setTila(JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI);


        Jarjestyskriteeritulos s1t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s1t2 = new Jarjestyskriteeritulos();
        s1t1.setArvo(new BigDecimal("2"));
        s1t2.setArvo(new BigDecimal("2"));
        sija1.getJarjestyskriteerit().put(1, s1t1);
        sija1.getJarjestyskriteerit().put(2, s1t2);

        Jarjestyskriteeritulos s2t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s2t2 = new Jarjestyskriteeritulos();
        s2t1.setArvo(new BigDecimal("2"));
        s2t2.setArvo(new BigDecimal("2"));
        sija2.getJarjestyskriteerit().put(1, s2t1);
        sija2.getJarjestyskriteerit().put(2, s2t2);

        Jarjestyskriteeritulos s3t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s3t2 = new Jarjestyskriteeritulos();
        s3t1.setArvo(new BigDecimal("4"));
        s3t2.setArvo(new BigDecimal("1"));
        sija3.getJarjestyskriteerit().put(1, s3t1);
        sija3.getJarjestyskriteerit().put(2, s3t2);

        Jarjestyskriteeritulos s4t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s4t2 = new Jarjestyskriteeritulos();
        s4t1.setArvo(new BigDecimal("3"));
        s4t2.setArvo(new BigDecimal("1"));
        sija4.getJarjestyskriteerit().put(1, s4t1);
        sija4.getJarjestyskriteerit().put(2, s4t2);

        for (Jonosija j : list) {
            System.out.println("Before sort: " + j.getHakemusoid());
        }

        JonosijaToJonosijaDTOConverter c = new JonosijaToJonosijaDTOConverter();
        List<JonosijaDTO> a = c.jarjestaJaLisaaJonosijaNumero(list);

        for (JonosijaDTO j : a) {
            System.out.println("After sort: " + j.getHakemusOid());
        }

        Assert.assertEquals(a.get(1).getHakemusOid(), sija3.getHakemusoid());
        Assert.assertEquals(a.get(2).getHakemusOid(), sija4.getHakemusoid());

    }

}
