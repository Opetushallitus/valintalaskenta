package fi.vm.sade.valintalaskenta.domain.comparator;

import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;

import java.util.Comparator;
import java.util.TreeSet;

/**
 * Created with IntelliJ IDEA. User: kkammone Date: 13.5.2013 Time: 14:13 To
 * change this template use File | Settings | File Templates.
 */
public class JonosijaDTOComparator implements Comparator<JonosijaDTO> {
    @Override
    public int compare(JonosijaDTO thiz, JonosijaDTO other) {

        boolean thizHarkinanvaraisestiHyvaksytty = (thiz.getTuloksenTila() != null && thiz.getTuloksenTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI) ||
                thiz.getJarjestyskriteerit().get(0) != null &&  thiz.getJarjestyskriteerit().get(0).getTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI;

        boolean otherHarkinanvaraisestiHyvaksytty = other.getTuloksenTila() != null && other.getTuloksenTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI ||
                other.getJarjestyskriteerit().get(0) != null &&  other.getJarjestyskriteerit().get(0).getTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI;


        //harkinanvaraisesti hyvaksytyt ovat aina listan karjessa.
        if( thizHarkinanvaraisestiHyvaksytty && otherHarkinanvaraisestiHyvaksytty) {
            //do nothing;
        }   else if(thizHarkinanvaraisestiHyvaksytty) {
            return -1;
        }   else if(otherHarkinanvaraisestiHyvaksytty) {
            return 1;
        }

        TreeSet<Integer> keys = new TreeSet<Integer>();
        keys.addAll(thiz.getJarjestyskriteerit().keySet());
        keys.addAll(other.getJarjestyskriteerit().keySet());
        for (Integer i : keys) {
            JarjestyskriteeritulosDTO thisValue = null;
            if (thiz.getJarjestyskriteerit().containsKey(i)) {
                thisValue = thiz.getJarjestyskriteerit().get(i);
            }
            JarjestyskriteeritulosDTO otherValue = null;
            if (other.getJarjestyskriteerit().containsKey(i)) {
                otherValue = other.getJarjestyskriteerit().get(i);
            }
            if ( (thisValue == null||thisValue.getArvo() ==null) && (otherValue == null || otherValue.getArvo() ==null) ) {
                continue;
            } else if (thisValue == null || thisValue.getArvo() == null) {
                return -1;
            } else if (otherValue == null|| otherValue.getArvo() == null) {
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
