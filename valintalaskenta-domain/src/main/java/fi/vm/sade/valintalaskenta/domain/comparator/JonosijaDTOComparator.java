package fi.vm.sade.valintalaskenta.domain.comparator;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;

import java.util.*;

/**
 * Created with IntelliJ IDEA. User: kkammone Date: 13.5.2013 Time: 14:13 To
 * change this template use File | Settings | File Templates.
 */
public class JonosijaDTOComparator implements Comparator<JonosijaDTO> {

    private Map<Integer, JarjestyskriteeritulosDTO> jarjestyskriteeritPrioriteetinMukaan(Collection<JarjestyskriteeritulosDTO> jks) {
        Map<Integer, JarjestyskriteeritulosDTO> map = new HashMap<Integer, JarjestyskriteeritulosDTO>();

        for (JarjestyskriteeritulosDTO dto : jks) {
            map.put(dto.getPrioriteetti(), dto);
        }

        return map;
    }

    @Override
    public int compare(JonosijaDTO thiz, JonosijaDTO other) {

        boolean thizHarkinanvaraisestiHyvaksytty = (thiz.getTuloksenTila() != null && thiz.getTuloksenTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI) ||
                !thiz.getJarjestyskriteerit().isEmpty() && thiz.getJarjestyskriteerit().get(0).getTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI;

        boolean otherHarkinanvaraisestiHyvaksytty = other.getTuloksenTila() != null && other.getTuloksenTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI ||
                !other.getJarjestyskriteerit().isEmpty() && other.getJarjestyskriteerit().get(0).getTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI;


        //harkinanvaraisesti hyvaksytyt ovat aina listan karjessa.
        if (thizHarkinanvaraisestiHyvaksytty && otherHarkinanvaraisestiHyvaksytty) {
            //do nothing;
        } else if (thizHarkinanvaraisestiHyvaksytty) {
            return -1;
        } else if (otherHarkinanvaraisestiHyvaksytty) {
            return 1;
        }
        Map<Integer, JarjestyskriteeritulosDTO> thizJarjestyskriteerit = jarjestyskriteeritPrioriteetinMukaan(thiz.getJarjestyskriteerit());
        Map<Integer, JarjestyskriteeritulosDTO> otherJarjestyskriteerit = jarjestyskriteeritPrioriteetinMukaan(other.getJarjestyskriteerit());

        TreeSet<Integer> keys = new TreeSet<Integer>();
        keys.addAll(thizJarjestyskriteerit.keySet());
        keys.addAll(otherJarjestyskriteerit.keySet());

        for (Integer i : keys) {
            JarjestyskriteeritulosDTO thisValue = null;
            if (thizJarjestyskriteerit.containsKey(i)) {
                thisValue = thizJarjestyskriteerit.get(i);
            }
            JarjestyskriteeritulosDTO otherValue = null;
            if (otherJarjestyskriteerit.containsKey(i)) {
                otherValue = otherJarjestyskriteerit.get(i);
            }
            if ((thisValue == null || thisValue.getArvo() == null) && (otherValue == null || otherValue.getArvo() == null)) {
                continue;
            } else if (thisValue == null || thisValue.getArvo() == null) {
                return -1;
            } else if (otherValue == null || otherValue.getArvo() == null) {
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
