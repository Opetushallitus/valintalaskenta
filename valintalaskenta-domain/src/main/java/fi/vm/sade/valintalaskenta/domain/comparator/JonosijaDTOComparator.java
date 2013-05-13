package fi.vm.sade.valintalaskenta.domain.comparator;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;

import java.util.Comparator;
import java.util.TreeSet;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 13.5.2013
 * Time: 14:13
 * To change this template use File | Settings | File Templates.
 */
public class JonosijaDTOComparator implements Comparator<JonosijaDTO>{
    @Override
    public int compare(JonosijaDTO thiz, JonosijaDTO other) {

        TreeSet<Integer> keys = new TreeSet<Integer>();
        keys.addAll(thiz.getJarjestyskriteerit().keySet());
        keys.addAll(other.getJarjestyskriteerit().keySet());
        for(Integer i : keys) {
            Jarjestyskriteeritulos thisValue =  null;
            if(thiz.getJarjestyskriteerit().containsKey(i)){
                thisValue = thiz.getJarjestyskriteerit().get(i);
            }
            Jarjestyskriteeritulos otherValue =  null;
            if(other.getJarjestyskriteerit().containsKey(i)){
                otherValue = thiz.getJarjestyskriteerit().get(i);
            }
            if(thisValue == null && otherValue == null) {
                continue;
            } else if(thisValue == null) {
                return -1;
            } else if(otherValue == null) {
                return 1;
            } else if(otherValue.getArvo() == thisValue.getArvo()) {
                continue;
            } else if(otherValue.getArvo() > thisValue.getArvo()) {
                return -1;
            }else if(otherValue.getArvo() < thisValue.getArvo()) {
                return  -1;
            }
        }
        return 0;  //To change body of implemented methods use File | Settings | File Templates.
    }

}

