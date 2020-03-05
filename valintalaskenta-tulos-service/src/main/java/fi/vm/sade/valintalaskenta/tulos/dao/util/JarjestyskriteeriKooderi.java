package fi.vm.sade.valintalaskenta.tulos.dao.util;

import fi.vm.sade.valintalaskenta.domain.GzipUtil;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;

/**
 * @author Jussi Jartamo
 */
public class JarjestyskriteeriKooderi {

    public static boolean tarvitseekoEnkoodata(Jarjestyskriteerihistoria j) {
        return j.getHistoria() != null;
    }

    public static Jarjestyskriteerihistoria dekoodaa(Jarjestyskriteerihistoria j) {
        if(j.getHistoriaGzip() != null && j.getHistoria() == null) {
            try {
                j.setHistoria(GzipUtil.dekoodaa(j.getHistoriaGzip()));
            } catch (Throwable t) {
                throw new RuntimeException("Historian unzippaaminen epaonnistui!",t);
            }
        }
        return j;
    }

    public static Jarjestyskriteerihistoria enkoodaa(Jarjestyskriteerihistoria j) {
        if(j.getHistoriaGzip() == null && j.getHistoria() != null) {
            try {
                j.setHistoriaGzip(GzipUtil.enkoodaa(j.getHistoria()));
                j.setHistoria(null);
            } catch (Throwable t) {
                throw new RuntimeException("Historian gzippaaminen epaonnistui!",t);
            }
        }
        return j;
    }
}
