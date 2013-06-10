package fi.vm.sade.service.valintaperusteet.algoritmi.domain.comparator;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.comparator.JonosijaComparator;
import junit.framework.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 *
 * Date: 7.6.2013
 * Time: 9:18
 * To change this template use File | Settings | File Templates.
 */

public class JonosijaComparatorTest {

    @Test
    public void testCompartor() {

        List<Jonosija> list = new ArrayList<Jonosija>();

        Jonosija sija1 = new Jonosija();
        sija1.setKuvaus("sija1");
        Jonosija sija2 = new Jonosija();
        sija2.setKuvaus("sija2");
        Jonosija sija3 = new Jonosija();
        sija3.setKuvaus("sija3");
        Jonosija sija4 = new Jonosija();
        sija4.setKuvaus("sija4");
        list.add(sija1);
        list.add(sija2);
        list.add(sija3);
        list.add(sija4);

        Jarjestyskriteeritulos s1t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s1t2 = new Jarjestyskriteeritulos();
        s1t1.setArvo(2);
        s1t2.setArvo(2);
        sija1.getJarjestyskriteerit().put(1, s1t1);
        sija1.getJarjestyskriteerit().put(2, s1t2);


        Jarjestyskriteeritulos s2t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s2t2 = new Jarjestyskriteeritulos();
        s2t1.setArvo(2);
        s2t2.setArvo(2);
        sija2.getJarjestyskriteerit().put(1, s2t1);
        sija2.getJarjestyskriteerit().put(2, s2t2);

        Jarjestyskriteeritulos s3t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s3t2 = new Jarjestyskriteeritulos();
        s3t1.setArvo(4);
        s3t2.setArvo(1);
        sija3.getJarjestyskriteerit().put(1, s3t1);
        sija3.getJarjestyskriteerit().put(2, s3t2);


        Jarjestyskriteeritulos s4t1 = new Jarjestyskriteeritulos();
        Jarjestyskriteeritulos s4t2 = new Jarjestyskriteeritulos();
        s4t1.setArvo(3);
        s4t2.setArvo(1);
        sija4.getJarjestyskriteerit().put(1, s4t1);
        sija4.getJarjestyskriteerit().put(2, s4t2);

        JonosijaComparator c = new JonosijaComparator();
        Collections.sort(list, c);

        for(Jonosija j : list) {
            System.out.println(j.getKuvaus());
        }

        Assert.assertEquals(list.get(0), sija3);
        Assert.assertEquals(list.get(1), sija4);

        Assert.assertEquals(0, c.compare(sija1, sija2));

    }

}
