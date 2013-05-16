package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.comparator.JonosijaComparator;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 16.5.2013
 * Time: 13:50
 * To change this template use File | Settings | File Templates.
 */
public class ValintatapajonoHelper {

    public static void sortJonosijat(List<Jonosija> jonosijat) {

        JonosijaComparator comparator = new JonosijaComparator();
        Collections.sort(jonosijat, comparator);

        int i = 1;
        Jonosija previous = null;
        Iterator<Jonosija> it = jonosijat.iterator();
        while(it.hasNext()) {
            Jonosija dto = it.next();
            if(previous != null && comparator.compare(previous, dto) != 0) {
                i++;
            }
            dto.setJonosija(i);
            previous = dto;
        }
    }
}
