package fi.vm.sade.valintalaskenta.domain.comparator;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;

import java.util.*;

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
        // Virheelliset parhaille jonosijoille, jotta niihin voidaan ottaa kantaa
        boolean thizVirhe = (thiz.getTuloksenTila() != null && thiz.getTuloksenTila() == JarjestyskriteerituloksenTila.VIRHE)
                || !thiz.getJarjestyskriteerit().isEmpty()
                && thiz.getJarjestyskriteerit().first().getTila() == JarjestyskriteerituloksenTila.VIRHE;

        boolean otherVirhe = other.getTuloksenTila() != null
                && other.getTuloksenTila() == JarjestyskriteerituloksenTila.VIRHE
                || !other.getJarjestyskriteerit().isEmpty()
                && other.getJarjestyskriteerit().first().getTila() == JarjestyskriteerituloksenTila.VIRHE;
        if (thizVirhe && otherVirhe) {
            // do nothing;
        } else if (thizVirhe) {
            return -1;
        } else if (otherVirhe) {
            return 1;
        }

        // harkinanvaraisesti hyvaksytyt ovat aina listan karjessa.
        boolean thizHarkinanvaraisestiHyvaksytty = (thiz.getTuloksenTila() != null && thiz.getTuloksenTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI)
                || !thiz.getJarjestyskriteerit().isEmpty()
                && thiz.getJarjestyskriteerit().first().getTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI;
        boolean otherHarkinanvaraisestiHyvaksytty = other.getTuloksenTila() != null
                && other.getTuloksenTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI
                || !other.getJarjestyskriteerit().isEmpty()
                && other.getJarjestyskriteerit().first().getTila() == JarjestyskriteerituloksenTila.HYVAKSYTTY_HARKINNANVARAISESTI;
        if (thizHarkinanvaraisestiHyvaksytty && otherHarkinanvaraisestiHyvaksytty) {
            // do nothing;
        } else if (thizHarkinanvaraisestiHyvaksytty) {
            return -1;
        } else if (otherHarkinanvaraisestiHyvaksytty) {
            return 1;
        }

        // hylatyt ovat aina jonon pohjalla
        boolean thizHylatty = (thiz.getTuloksenTila() != null && thiz.getTuloksenTila() == JarjestyskriteerituloksenTila.HYLATTY)
                || !thiz.getJarjestyskriteerit().isEmpty()
                && thiz.getJarjestyskriteerit().first().getTila() == JarjestyskriteerituloksenTila.HYLATTY;

        boolean otherHylatty = other.getTuloksenTila() != null
                && other.getTuloksenTila() == JarjestyskriteerituloksenTila.HYLATTY
                || !other.getJarjestyskriteerit().isEmpty()
                && other.getJarjestyskriteerit().first().getTila() == JarjestyskriteerituloksenTila.HYLATTY;
        if (thizHylatty && otherHylatty) {
            // do nothing;
        } else if (thizHylatty) {
            return 1;
        } else if (otherHylatty) {
            return -1;
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
            if ((thisValue == null || thisValue.getArvo() == null)
                    && (otherValue == null || otherValue.getArvo() == null)) {
                continue;
            } else if (thisValue == null || thisValue.getArvo() == null) {
                return 1;
            } else if (otherValue == null || otherValue.getArvo() == null) {
                return -1;
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
