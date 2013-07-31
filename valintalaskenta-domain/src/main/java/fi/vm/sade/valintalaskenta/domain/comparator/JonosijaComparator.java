package fi.vm.sade.valintalaskenta.domain.comparator;

import java.util.Comparator;
import java.util.TreeSet;

import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Jonosija;

/**
 * Created with IntelliJ IDEA. User: kkammone Date: 13.5.2013 Time: 14:13 To
 * change this template use File | Settings | File Templates.
 */
public class JonosijaComparator implements Comparator<Jonosija> {
    @Override
    public int compare(Jonosija thiz, Jonosija other) {

        boolean thizHarkinanvaraisestiHyvaksytty = (thiz.getTuloksenTila() != null && thiz.getTuloksenTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI) ||
                thiz.getJarjestyskriteerit().get(1) != null &&  thiz.getJarjestyskriteerit().get(1).getTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI;

        boolean otherHarkinanvaraisestiHyvaksytty = other.getTuloksenTila() != null && other.getTuloksenTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI ||
                other.getJarjestyskriteerit().get(1) != null &&  other.getJarjestyskriteerit().get(1).getTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI;


        //harkinanvaraisesti hyvaksytyt ovat aina listan karjessa.
        if( thizHarkinanvaraisestiHyvaksytty && otherHarkinanvaraisestiHyvaksytty) {
            //do nothing;
        }   else if(thizHarkinanvaraisestiHyvaksytty) {
            return 1;
        }   else if(otherHarkinanvaraisestiHyvaksytty) {
            return -1;
        }

        TreeSet<Integer> keys = new TreeSet<Integer>();
        keys.addAll(thiz.getJarjestyskriteerit().keySet());
        keys.addAll(other.getJarjestyskriteerit().keySet());
        for (Integer i : keys) {
            Jarjestyskriteeritulos thisValue = null;
            if (thiz.getJarjestyskriteerit().containsKey(i)) {
                thisValue = thiz.getJarjestyskriteerit().get(i);
            }
            Jarjestyskriteeritulos otherValue = null;
            if (other.getJarjestyskriteerit().containsKey(i)) {
                otherValue = other.getJarjestyskriteerit().get(i);
            }
            if (thisValue == null && otherValue == null) {
                continue;
            } else if (thisValue == null) {
                return -1;
            } else if (otherValue == null) {
                return 1;
            } else if (otherValue.getArvo().equals(thisValue.getArvo())) {
                continue;
            } else if (otherValue.getArvo().compareTo(thisValue.getArvo()) == 1) {
                return 1;
            } else if (otherValue.getArvo().compareTo(thisValue.getArvo()) == -1) {
                return -1;
            }
        }
        return 0;
    }

}
