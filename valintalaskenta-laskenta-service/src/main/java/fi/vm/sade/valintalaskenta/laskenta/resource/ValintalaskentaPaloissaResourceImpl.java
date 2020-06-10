package fi.vm.sade.valintalaskenta.laskenta.resource;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.Laskentakutsu;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import java.util.concurrent.ConcurrentHashMap;

@Controller
@Path("valintalaskentaPaloissa")
public class ValintalaskentaPaloissaResourceImpl {
    private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaPaloissaResourceImpl.class);

    private static volatile ConcurrentHashMap<String, Laskentakutsu> siirrettavanaOlevatLaskentakutsut;
    private final ValintalaskentaResourceImpl valintalaskentaResource;

    @Autowired
    public ValintalaskentaPaloissaResourceImpl(ValintalaskentaResourceImpl valintalaskentaResource) {
        this.valintalaskentaResource = valintalaskentaResource;
        siirrettavanaOlevatLaskentakutsut = new ConcurrentHashMap<>();
    }

    @Path("aloita/{key}")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String aloita(@PathParam("key") String pollKey, Laskentakutsu laskentakutsu) {
        LOG.info(String.format("Saatiin pyyntö aloittaa laskentakutsu %s. Merkitään se muistiin.", laskentakutsu.getPollKey()));
        siirrettavanaOlevatLaskentakutsut.compute(laskentakutsu.getPollKey(), (key, aiemminTallennetuKutsu) -> {
            if (aiemminTallennetuKutsu != null) {
                throw new IllegalStateException(String.format("Yritettiin aloittaa laskentakutsu %s uudestaan!", laskentakutsu.getPollKey()));
            }
            return laskentakutsu;
        });
        return "Tallennnettiin muistiin tieto siitä, että laskennan " + laskentakutsu.getPollKey() + " siirtäminen paloissa alkaa.";
    }

    @Path("lisaaHakukohde/{key}")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String lisaaHakukohde(@PathParam("key") String pollKey, LaskeDTO laskeDto) {
        LOG.info(String.format("Saatiin pyyntö lisätä hakukohde %s laskentakutsuun %s.", laskeDto.getHakukohdeOid(), pollKey));
        siirrettavanaOlevatLaskentakutsut.compute(pollKey, (key, kutsu) -> {
            if (kutsu == null) {
                throw new IllegalStateException(String.format("Siirrettävänä oli laskentakutsut %s, mutta yritettiin lisätä hakukohde kutsuun %s!",
                    siirrettavanaOlevatLaskentakutsut.keys(), pollKey));
            }
            kutsu.lisaaLaskeDto(laskeDto);
            return kutsu;
        });
        return "Lisättiin laskentakutsuun " + pollKey + " hakukohteen " + laskeDto.getHakukohdeOid() + " laskentaresurssit.";
    }

    @Path("lisaaSuoritustiedot/{key}")
    @Consumes("text/plain")
    @Produces("text/plain")
    @POST
    public String lisaaSuoritustiedot(@PathParam("key") String pollKey, String suoritustiedotDtoBase64Gzip) {
        LOG.info(String.format("Saatiin pyyntö lisätä suoritustiedot laskentakutsuun %s.", pollKey));
        siirrettavanaOlevatLaskentakutsut.compute(pollKey, (key, kutsu) -> {
            if (kutsu == null) {
                throw new IllegalStateException(String.format("Siirrettävänä oli laskentakutsut %s, mutta yritettiin lisätä suoritustiedot kutsuun %s!",
                    siirrettavanaOlevatLaskentakutsut.keys(), pollKey));
            }
            kutsu.setSuoritustiedotDtoBase64Gzip(suoritustiedotDtoBase64Gzip);
            return kutsu;
        });
        return String.format("Lisättiin laskentakutsuun %s suoritustiedot.", pollKey);
    }

    @Path("kaynnista/{key}")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String kaynnista(@PathParam("key") String pollKey, Laskentakutsu laskentakutsu) {
        LOG.info(String.format("Saatiin pyyntö aloittaa laskenta %s.", pollKey));
        final Laskentakutsu populoituKutsu = siirrettavanaOlevatLaskentakutsut.remove(pollKey);

        if (populoituKutsu == null) {
            throw new IllegalStateException(String.format("Siirrettävänä oli laskentakutsut %s, mutta yritettiin alkaa laskea kutsua %s!",
                siirrettavanaOlevatLaskentakutsut.keys(), pollKey));
        }
        return valintalaskentaResource.laskeJaSijoittele(populoituKutsu);
    }
}
