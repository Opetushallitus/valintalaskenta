package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import org.apache.commons.io.IOUtils;
import org.mongodb.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.PostLoad;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

@Repository("jonosijaHistoriaDAO")
public class JarjestyskriteerihistoriaDAOImpl implements JarjestyskriteerihistoriaDAO {
    private static final Logger LOG = LoggerFactory.getLogger(JarjestyskriteerihistoriaDAOImpl.class);
    @Autowired
    private Datastore datastore;

    @Override
    public void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
        datastore.save(zip(jarjestyskriteerihistoria));
    }

    @Override
    public void delete(ObjectId id) {
        datastore.delete(Jarjestyskriteerihistoria.class, id);
    }

    @Override
    public Jarjestyskriteerihistoria hae(ObjectId id) {
        Jarjestyskriteerihistoria h = datastore.find(Jarjestyskriteerihistoria.class).field("_id").equal(id).get();
        if(h.getHistoria() != null) {
            create(h);// Poistaa luettaessa zippaamattomat historiat ja persistoi zipattuna
        }
        return unzip(h);
    }

    private Jarjestyskriteerihistoria zip(Jarjestyskriteerihistoria j) {
        if(j.getHistoriaGzip() == null && j.getHistoria() != null) {
            try {
                ByteArrayOutputStream b = new ByteArrayOutputStream();
                GZIPOutputStream g = new GZIPOutputStream(b);
                IOUtils.write(j.getHistoria().getBytes(), g);
                IOUtils.closeQuietly(g);
                j.setHistoriaGzip(b.toByteArray());
                j.setHistoria(null);
            } catch (Throwable t) {
                LOG.error("Historian gzippaaminen epaonnistui!",t);
                throw new RuntimeException("Historian gzippaaminen epaonnistui!",t);
            }
        }
        return j;
    }

    private Jarjestyskriteerihistoria unzip(Jarjestyskriteerihistoria j) {
        if(j.getHistoriaGzip() != null && j.getHistoria() == null) {
            try {
                j.setHistoria(IOUtils.toString(new GZIPInputStream(new ByteArrayInputStream(j.getHistoriaGzip()))));
            } catch (Throwable t) {
                LOG.error("Historian unzippaaminen epaonnistui!",t);
                throw new RuntimeException("Historian unzippaaminen epaonnistui!",t);
            }
        }
        return j;
    }
}
