package fi.vm.sade.valintalaskenta.laskenta.resource;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.Laskentakutsu;
import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping(value = "/valintalaskentaPaloissa")
public class ValintalaskentaPaloissaResourceImpl {
  private static final Logger LOG =
      LoggerFactory.getLogger(ValintalaskentaPaloissaResourceImpl.class);

  private static volatile ConcurrentHashMap<String, Laskentakutsu>
      siirrettavanaOlevatLaskentakutsut;
  private final ValintalaskentaResourceImpl valintalaskentaResource;

  @Autowired
  public ValintalaskentaPaloissaResourceImpl(ValintalaskentaResourceImpl valintalaskentaResource) {
    this.valintalaskentaResource = valintalaskentaResource;
    siirrettavanaOlevatLaskentakutsut = new ConcurrentHashMap<>();
  }

  @PostMapping(
      value = "/aloita/{key}",
      produces = MediaType.TEXT_PLAIN_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public String aloita(
      @PathVariable("key") final String pollKey, @RequestBody final Laskentakutsu laskentakutsu) {
    LOG.info(
        String.format(
            "Saatiin pyyntö aloittaa laskentakutsu %s. Merkitään se muistiin.",
            laskentakutsu.getPollKey()));
    siirrettavanaOlevatLaskentakutsut.compute(
        laskentakutsu.getPollKey(),
        (key, aiemminTallennetuKutsu) -> {
          if (aiemminTallennetuKutsu != null) {
            throw new IllegalStateException(
                String.format(
                    "Yritettiin aloittaa laskentakutsu %s uudestaan!", laskentakutsu.getPollKey()));
          }
          return laskentakutsu;
        });
    return "Tallennnettiin muistiin tieto siitä, että laskennan "
        + laskentakutsu.getPollKey()
        + " siirtäminen paloissa alkaa.";
  }

  @PostMapping(
      value = "/lisaaHakukohde/{key}",
      produces = MediaType.TEXT_PLAIN_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public String lisaaHakukohde(
      @PathVariable("key") final String pollKey, @RequestBody final LaskeDTO laskeDto) {
    LOG.info(
        String.format(
            "Saatiin pyyntö lisätä hakukohde %s laskentakutsuun %s.",
            laskeDto.getHakukohdeOid(), pollKey));
    siirrettavanaOlevatLaskentakutsut.compute(
        pollKey,
        (key, kutsu) -> {
          if (kutsu == null) {
            throw new IllegalStateException(
                String.format(
                    "Siirrettävänä oli laskentakutsut %s, mutta yritettiin lisätä hakukohde kutsuun %s!",
                    siirrettavanaOlevatLaskentakutsut.keys(), pollKey));
          }
          kutsu.lisaaLaskeDto(laskeDto);
          return kutsu;
        });
    return "Lisättiin laskentakutsuun "
        + pollKey
        + " hakukohteen "
        + laskeDto.getHakukohdeOid()
        + " laskentaresurssit.";
  }

  @PostMapping(
      value = "/lisaaSuoritustiedot/{key}",
      produces = MediaType.TEXT_PLAIN_VALUE,
      consumes = MediaType.TEXT_PLAIN_VALUE)
  public String lisaaSuoritustiedot(
      @PathVariable("key") final String pollKey,
      @RequestBody final String suoritustiedotDtoBase64Gzip) {
    LOG.info(String.format("Saatiin pyyntö lisätä suoritustiedot laskentakutsuun %s.", pollKey));
    siirrettavanaOlevatLaskentakutsut.compute(
        pollKey,
        (key, kutsu) -> {
          if (kutsu == null) {
            throw new IllegalStateException(
                String.format(
                    "Siirrettävänä oli laskentakutsut %s, mutta yritettiin lisätä suoritustiedot kutsuun %s!",
                    siirrettavanaOlevatLaskentakutsut.keys(), pollKey));
          }
          kutsu.setSuoritustiedotDtoBase64Gzip(suoritustiedotDtoBase64Gzip);
          return kutsu;
        });
    return String.format("Lisättiin laskentakutsuun %s suoritustiedot.", pollKey);
  }

  @PostMapping(
      value = "/kaynnista/{key}",
      produces = MediaType.TEXT_PLAIN_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public String kaynnista(
      @PathVariable("key") final String pollKey, @RequestBody final Laskentakutsu laskentakutsu) {
    LOG.info(String.format("Saatiin pyyntö aloittaa laskenta %s.", pollKey));
    final Laskentakutsu populoituKutsu = siirrettavanaOlevatLaskentakutsut.remove(pollKey);

    if (populoituKutsu == null) {
      throw new IllegalStateException(
          String.format(
              "Siirrettävänä oli laskentakutsut %s, mutta yritettiin alkaa laskea kutsua %s!",
              siirrettavanaOlevatLaskentakutsut.keys(), pollKey));
    }
    return valintalaskentaResource.laskeJaSijoittele(populoituKutsu);
  }
}
