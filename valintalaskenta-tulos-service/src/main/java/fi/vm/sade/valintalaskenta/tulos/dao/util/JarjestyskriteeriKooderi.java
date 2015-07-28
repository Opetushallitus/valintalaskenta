package fi.vm.sade.valintalaskenta.tulos.dao.util;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import org.apache.commons.io.IOUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

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
                j.setHistoria(IOUtils.toString(new GZIPInputStream(new ByteArrayInputStream(j.getHistoriaGzip()))));
            } catch (Throwable t) {
                throw new RuntimeException("Historian unzippaaminen epaonnistui!",t);
            }
        }
        return j;
    }

    public static Jarjestyskriteerihistoria enkoodaa(Jarjestyskriteerihistoria j) {
        if(j.getHistoriaGzip() == null && j.getHistoria() != null) {
            try {
                ByteArrayOutputStream b = new ByteArrayOutputStream();
                GZIPOutputStream g = new GZIPOutputStream(b);
                IOUtils.write(j.getHistoria().getBytes(), g);
                IOUtils.closeQuietly(g);
                j.setHistoriaGzip(b.toByteArray());
                j.setHistoria(null);
            } catch (Throwable t) {
                throw new RuntimeException("Historian gzippaaminen epaonnistui!",t);
            }
        }
        return j;
    }
}
