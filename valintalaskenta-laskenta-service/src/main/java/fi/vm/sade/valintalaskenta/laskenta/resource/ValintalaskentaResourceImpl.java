package fi.vm.sade.valintalaskenta.laskenta.resource;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO;
import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import fi.vm.sade.valintalaskenta.domain.HakukohteenLaskennanTila;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.Laskentakutsu;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import javax.ws.rs.*;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

@Controller
@Path("valintalaskenta")
public class ValintalaskentaResourceImpl {
    private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaResourceImpl.class);

    private final ValintalaskentaService valintalaskentaService;
    private final ValisijoitteluKasittelija valisijoitteluKasittelija;
    private final ValiSijoitteluResource valiSijoitteluResource;
    private final ErillisSijoitteluResource erillisSijoitteluResource;
    private final ValintaperusteetValintatapajonoResource valintatapajonoResource;
    private final ExecutorService executorService;

    private static volatile ConcurrentHashMap<String, String> hakukohteetLaskettavina;

    @Autowired
    public ValintalaskentaResourceImpl(ValintalaskentaService valintalaskentaService, ValisijoitteluKasittelija valisijoitteluKasittelija,
                                       ValiSijoitteluResource valiSijoitteluResource, ErillisSijoitteluResource erillisSijoitteluResource,
                                       ValintaperusteetValintatapajonoResource valintatapajonoResource) {
        this.valintalaskentaService = valintalaskentaService;
        this.valisijoitteluKasittelija = valisijoitteluKasittelija;
        this.valiSijoitteluResource = valiSijoitteluResource;
        this.erillisSijoitteluResource = erillisSijoitteluResource;
        this.valintatapajonoResource = valintatapajonoResource;

        hakukohteetLaskettavina = new ConcurrentHashMap<>();
        this.executorService = Executors.newWorkStealingPool();
    }

    @Path("status/{key}")
    @Produces("text/plain")
    @GET
    public String status(@PathParam("key") String pollKey) throws Exception {
        try {
            return pidaKirjaaMeneillaanOlevista(pollKey, false);
        } catch (Exception e) {
            LOG.error("Odottamaton virhe : pidaKirjaaMeneillaanOlevista", e);
            throw e;
        }
    }

    @Path("laske")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laske(Laskentakutsu laskentakutsu) throws Exception {
        try {
            String pollKey = laskentakutsu.getPollKey();

            String status = pidaKirjaaMeneillaanOlevista(pollKey);
            if (!status.equals(HakukohteenLaskennanTila.UUSI)) {
                return status;
            } else {
                //Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran samalle tunnisteelle vaikka pyyntöjä tulisi useita
                try {
                    executorService.submit(timeRunnable.apply(laskentakutsu.getUuid(), () -> toteutaLaskenta(laskentakutsu)));
                    return HakukohteenLaskennanTila.UUSI;
                } catch (Exception e) {
                    LOG.error("Virhe laskennan suorituksessa, ", e);
                    throw e;
                }
            }
        } catch (Exception e) {
            LOG.error("Odottamaton virhe : laske(Laskentakutsu laskentakutsu)", e);
            throw e;
        }
    }

    @Path("valintakokeet")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String valintakokeet(Laskentakutsu laskentakutsu) throws Exception {
        try {
            String pollKey = laskentakutsu.getPollKey();

            String status = pidaKirjaaMeneillaanOlevista(pollKey);
            if (!status.equals(HakukohteenLaskennanTila.UUSI)) {
                return status;
            } else {
                //Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran samalle tunnisteelle vaikka pyyntöjä tulisi useita
                try {
                    executorService.submit(timeRunnable.apply(laskentakutsu.getUuid(), () -> toteutaValintakoeLaskenta(laskentakutsu)));
                    return HakukohteenLaskennanTila.UUSI;
                } catch (Exception e) {
                    LOG.error("Virhe laskennan suorituksessa, ", e);
                    throw e;
                }
            }
        } catch (Exception e) {
            LOG.error("Odottamaton virhe: valintakokeet(Laskentakutsu laskentakutsu)", e);
            throw e;
        }
    }

    @Path("laskekaikki")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laskeKaikki(Laskentakutsu laskentakutsu) throws Exception {
        try {
            String pollKey = laskentakutsu.getPollKey();

            String status = pidaKirjaaMeneillaanOlevista(pollKey);
            if (!status.equals(HakukohteenLaskennanTila.UUSI)) {
                return status;
            } else {
                //Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran samalle tunnisteelle vaikka pyyntöjä tulisi useita
                try {
                    executorService.submit(timeRunnable.apply(laskentakutsu.getUuid(), () -> toteutaLaskeKaikki(laskentakutsu)));
                    return HakukohteenLaskennanTila.UUSI;
                } catch (Exception e) {
                    LOG.error("Virhe laskennan suorituksessa, ", e);
                    throw e;
                }
            }
        } catch (Exception e) {
            LOG.error("Odottamaton virhe: laskeKaikki(Laskentakutsu laskentakutsu)", e);
            throw e;
        }
    }

    @Path("laskejasijoittele")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laskeJaSijoittele(Laskentakutsu laskentakutsu) {
        try {
            List<LaskeDTO> lista = laskentakutsu.getLaskeDTOs();
            String pollKey = laskentakutsu.getPollKey();
            if (lista == null || lista.isEmpty()) {
                LOG.error("Laskejasijoittele-rajapinta sai syötteeksi tyhjän listan, joten laskentaa ei voida toteuttaa. lopetetaan.");
                return HakukohteenLaskennanTila.VIRHE;
            }
            String status = pidaKirjaaMeneillaanOlevista(pollKey);
            if (!status.equals(HakukohteenLaskennanTila.UUSI)) {
                return status;
            } else {
                //Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran samalle tunnisteelle vaikka pyyntöjä tulisi useita
                try {
                    executorService.submit(timeRunnable.apply(laskentakutsu.getUuid(), () -> toteutaLaskeJaSijoittele(laskentakutsu)));
                    return HakukohteenLaskennanTila.UUSI;
                } catch (Exception e) {
                    LOG.error("Virhe laskennan suorituksessa, ", e);
                    throw e;
                }
            }
        } catch (Exception e) {
            LOG.error("Odottamaton virhe: laskeJaSijoittele(Laskentakutsu laskentakutsu) ", e);
            throw e;
        }
    }

    private void erillisSijoittele(LaskeDTO laskeDTO, ValintaperusteetDTO valintaperusteetDTO) {
        List<String> jonot = valintaperusteetDTO.getValinnanVaihe().getValintatapajono().stream().map(j -> j.getOid()).collect(Collectors.toList());
        Map<String, List<String>> kohteet = new HashMap<>();
        kohteet.put(valintaperusteetDTO.getHakukohdeOid(), jonot);
        erillissijoitteleJonot(laskeDTO, kohteet);
    }

    private Map<String, List<String>> haeKopiotValintaperusteista(List<String> jonot) {
        return valintatapajonoResource.findKopiot(jonot);
    }

    private void valisijoitteleKopiot(LaskeDTO laskeDTO, Map<String, List<String>> valisijoiteltavatJonot) {
        String hakuoid = getHakuOid(laskeDTO);
        ValisijoitteluDTO dto = new ValisijoitteluDTO();
        dto.setHakukohteet(valisijoiteltavatJonot);
        List<HakukohdeDTO> sijoitellut = valiSijoitteluResource.sijoittele(hakuoid, dto);

        Map<String, HakemusDTO> hakemusHashMap = new ConcurrentHashMap<>();

        // Muodostetaan Map hakemuksittain sijoittelun tuloksista
        sijoitellut.parallelStream().forEach(hakukohde ->
            hakukohde.getValintatapajonot().parallelStream().forEach(valintatapajono ->
                valintatapajono.getHakemukset().parallelStream().forEach(hakemus ->
                    hakemusHashMap.put(
                        hakukohde.getOid() + valintatapajono.getOid()
                            + hakemus.getHakemusOid(), hakemus)
                )
            )
        );

        valintalaskentaService.applyValisijoittelu(valisijoiteltavatJonot, hakemusHashMap);
    }

    private String getHakuOid(final LaskeDTO laskeDTO) {
        String hakuoid;
        try {
            hakuoid = laskeDTO.getValintaperuste().get(0).getHakuOid();
        } catch (Exception e) {
            LOG.error("Välisijoittelulle ei löytynyt hakuoidia! uuid=" + laskeDTO.getUuid(), e);
            throw e;
        }
        return hakuoid;
    }

    private void erillissijoitteleJonot(LaskeDTO laskeDTO, Map<String, List<String>> jonot) {
        String hakuoid;
        try {
            hakuoid = laskeDTO.getValintaperuste().get(0).getHakuOid();
        } catch (Exception e) {
            LOG.error("Erillissijoittelulle ei löytynyt hakuoidia! uuid=" + laskeDTO.getUuid(), e);
            throw e;
        }
        ValisijoitteluDTO dto = new ValisijoitteluDTO();
        dto.setHakukohteet(jonot);
        Long ajo = erillisSijoitteluResource.sijoittele(hakuoid, dto);
        if (ajo == null) {
            throw new RuntimeException("Erillissijoittelu haulle " + hakuoid + " näyttää epäonnistuneen, koska rajapinta palautti null");
        }

        valintalaskentaService.applyErillissijoittelu(jonot, ajo);
    }

    private String pidaKirjaaMeneillaanOlevista(String pollKey) {
        return pidaKirjaaMeneillaanOlevista(pollKey, true);
    }

    private String pidaKirjaaMeneillaanOlevista(String pollKey, boolean luoJosPuuttuu) {
        if (!hakukohteetLaskettavina.containsKey(pollKey)) {
            if (luoJosPuuttuu) {
                LOG.info("Luodaan uusi laskettava hakukohde. {}", pollKey);
                hakukohteetLaskettavina.put(pollKey, HakukohteenLaskennanTila.KESKEN);
                return HakukohteenLaskennanTila.UUSI;
            } else {
                LOG.error("Haettiin statusta laskennalle {}, mutta laskentaa ei ole olemassa. Onko palvelin käynnistetty välissä uudelleen?", pollKey);
                return HakukohteenLaskennanTila.VIRHE;
            }
        } else if (hakukohteetLaskettavina.get(pollKey).equals(HakukohteenLaskennanTila.VALMIS)) {
            LOG.info("Kohteen laskenta on valmistunut. {}", pollKey);
            //hakukohteetLaskettavina.remove(key); //!! Joissain tilanteissa käynnistetään tarpeettomasti uusi laskenta
            return HakukohteenLaskennanTila.VALMIS;
        } else if (hakukohteetLaskettavina.get(pollKey).equals(HakukohteenLaskennanTila.VIRHE)) {
            LOG.error("Kohteen laskennassa on tapahtunut virhe. {}", pollKey);
            return HakukohteenLaskennanTila.VIRHE;
        } else {
            LOG.info("Hakukohteen laskenta on edelleen kesken. {}", pollKey);
            return HakukohteenLaskennanTila.KESKEN;
        }
    }

    private void paivitaKohteenLaskennanTila(String pollKey, String tila) {
        if (hakukohteetLaskettavina.containsKey(pollKey)) {
            if (!hakukohteetLaskettavina.get(pollKey).equals(HakukohteenLaskennanTila.KESKEN)) {
                LOG.error("Yritettiin päivittää sellaisen laskennan tilaa, joka ei ollut tilassa KESKEN. Ei päivitetty. pollKey: {}, tila nyt: ", pollKey, tila);
                return;
            }
        }
        if (tila.equals(HakukohteenLaskennanTila.VALMIS)) {
            LOG.info("Ollaan valmiita, merkataan kohteen laskennan tila valmiiksi. {} {}", pollKey);
        }
        if (hakukohteetLaskettavina.containsKey(pollKey)) {
            hakukohteetLaskettavina.replace(pollKey, tila);
        }
    }

    private boolean isErillisHaku(LaskeDTO laskeDTO, ValintaperusteetDTO valintaperusteetDTO) {
        return laskeDTO.isErillishaku()
            && valintaperusteetDTO.getViimeinenValinnanvaihe()
            == valintaperusteetDTO.getValinnanVaihe().getValinnanVaiheJarjestysluku();
    }

    private void toteutaLaskenta(Laskentakutsu laskentakutsu) {
        LaskeDTO laskeDTO = laskentakutsu.getLaskeDTO();
        String pollkey = laskentakutsu.getPollKey();

        LOG.info("(Uuid={}) Aloitetaan laskenta hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
        ValisijoitteluKasittelija.ValisijoiteltavatJonot valisijoiteltavatJonot = valisijoitteluKasittelija.valisijoiteltavatJonot(Arrays.asList(laskeDTO));
        if (!valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
            valisijoiteltavatJonot = new ValisijoitteluKasittelija.ValisijoiteltavatJonot(valisijoiteltavatJonot.valinnanvaiheet, haeKopiotValintaperusteista(valisijoiteltavatJonot.jonot.get(laskeDTO.getHakukohdeOid())));
        }
        try {
            ValintaperusteetDTO valintaperusteetDTO = laskeDTO.getValintaperuste().get(0);
            boolean erillisHaku = isErillisHaku(laskeDTO, valintaperusteetDTO);

            if (erillisHaku) {
                setSijoittelunKayttamanKentat(valintaperusteetDTO);
            }
            LOG.info("(Uuid={}) Suoritetaan laskenta. Hakemuksia {} kpl ja valintaperusteita {} kpl",
                laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getValintaperuste().size());
            valintalaskentaService.laske(laskeDTO.getHakemus(),
                laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(),
                laskeDTO.getHakukohdeOid(), laskeDTO.getUuid(), laskeDTO.isKorkeakouluhaku());
            if (erillisHaku) {
                erillisSijoittele(laskeDTO, valintaperusteetDTO);
            }
            LOG.info("(Uuid={}) Laskenta suoritettu hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
            paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VALMIS);
        } catch (Exception e) {
            LOG.error("Valintalaskenta epaonnistui! uuid=" + laskeDTO.getUuid(), e);
            paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VIRHE);
        }
        if (!valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
            valisijoitteleKopiot(laskeDTO, valisijoiteltavatJonot.jonot);
        }
    }

    private void toteutaValintakoeLaskenta(Laskentakutsu laskentakutsu) {
        LaskeDTO laskeDTO = laskentakutsu.getLaskeDTO();
        String pollkey = laskentakutsu.getPollKey();

        try {
            LOG.info("(Uuid={}) Suoritetaan valintakoelaskenta {} hakemukselle hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getHakukohdeOid());
            laskeDTO.getHakemus().forEach(h -> valintalaskentaService.valintakokeet(h, laskeDTO.getValintaperuste(), laskeDTO.getUuid(), new ValintakoelaskennanKumulatiivisetTulokset(), laskeDTO.isKorkeakouluhaku()));
            LOG.info("(Uuid={}) Valintakoelaskenta suoritettu {} hakemukselle hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getHakukohdeOid());
            paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VALMIS);
        } catch (Exception e) {
            LOG.error("Valintakoelaskenta epaonnistui! uuid=" + laskeDTO.getUuid(), e);
            paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VIRHE);
        }
    }

    public void toteutaLaskeKaikki(Laskentakutsu laskentakutsu) {
        LaskeDTO laskeDTO = laskentakutsu.getLaskeDTO();
        String pollkey = laskentakutsu.getPollKey();

        LOG.info("(Uuid={}) Aloitetaan laskenta hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
        ValisijoitteluKasittelija.ValisijoiteltavatJonot valisijoiteltavatJonot = valisijoitteluKasittelija.valisijoiteltavatJonot(Arrays.asList(laskeDTO));
        try {
            if (valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
                if (laskeDTO.isErillishaku()) {
                    laskeDTO.getValintaperuste().forEach(v -> {
                        if (v.getValinnanVaihe().getValinnanVaiheJarjestysluku() == v.getViimeinenValinnanvaihe()) {
                            setSijoittelunKayttamanKentat(v);
                        }
                    });
                }

                valintalaskentaService.laskeKaikki(laskeDTO.getHakemus(),
                    laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(),
                    laskeDTO.getHakukohdeOid(), laskeDTO.getUuid(), laskeDTO.isKorkeakouluhaku());
                if (laskeDTO.isErillishaku()) {
                    laskeDTO.getValintaperuste().stream()
                        .filter(v -> v.getValinnanVaihe().getValinnanVaiheJarjestysluku() == v.getViimeinenValinnanvaihe())
                        .forEach(v -> {
                            erillisSijoittele(laskeDTO, v);
                        });
                }
            } else {
                Map<Integer, List<LaskeDTO>> map = new TreeMap<>();
                laskeDTO.getValintaperuste().forEach(v -> {
                    List<LaskeDTO> dtos = map.getOrDefault(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), new ArrayList<>());
                    dtos.add(new LaskeDTO(laskeDTO.getUuid(), laskeDTO.isKorkeakouluhaku(), laskeDTO.isErillishaku(), laskeDTO.getHakukohdeOid(), laskeDTO.getHakemus(), Arrays.asList(v), laskeDTO.getHakijaryhmat()));
                    map.put(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), dtos);
                });

                ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset = new ValintakoelaskennanKumulatiivisetTulokset();
                map.keySet().stream().forEachOrdered(key -> {
                    map.get(key).forEach(dto -> {
                        ValintaperusteetValinnanVaiheDTO valinnanVaihe = dto.getValintaperuste().get(0).getValinnanVaihe();
                        if (valinnanVaihe.getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE)) {
                            LOG.info("(Uuid={}) Suoritetaan valintakoelaskenta {} hakemukselle", laskeDTO.getUuid(), laskeDTO.getHakemus().size());
                            laskeDTO.getHakemus().forEach(h -> valintalaskentaService.valintakokeet(h, dto.getValintaperuste(), laskeDTO.getUuid(), kumulatiivisetTulokset, laskeDTO.isKorkeakouluhaku()));
                        } else {
                            ValintaperusteetDTO valintaperusteetDTO = dto.getValintaperuste().get(0);
                            boolean erillisHaku = isErillisHaku(laskeDTO, valintaperusteetDTO);

                            if (erillisHaku) {
                                // Aseta sijoittelun käyttämät kentät
                                setSijoittelunKayttamanKentat(valintaperusteetDTO);
                            }
                            LOG.info("(Uuid={}) Suoritetaan laskenta. Hakemuksia {} kpl ja valintaperusteita {} kpl",
                                laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getValintaperuste().size());
                            valintalaskentaService.laske(dto.getHakemus(), dto.getValintaperuste(), dto.getHakijaryhmat(), dto.getHakukohdeOid(), laskeDTO.getUuid(), laskeDTO.isKorkeakouluhaku());

                            if (erillisHaku) {
                                erillisSijoittele(dto, valintaperusteetDTO);
                            }
                        }
                    });
                    if (valisijoiteltavatJonot.valinnanvaiheet.contains(key)) {
                        valisijoitteleKopiot(laskeDTO, new ImmutablePair<>(valisijoiteltavatJonot.valinnanvaiheet, haeKopiotValintaperusteista(valisijoiteltavatJonot.jonot.get(laskeDTO.getHakukohdeOid()))).getRight());
                    }
                });
            }
            LOG.info("(Uuid={}) Laskenta suoritettu hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
            paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VALMIS);
        } catch (Exception e) {
            LOG.error("Valintalaskenta ja valintakoelaskenta epaonnistui! uuid=" + laskeDTO.getUuid(), e);
            paivitaKohteenLaskennanTila(pollkey, HakukohteenLaskennanTila.VIRHE);
        }
    }

    private void toteutaLaskeJaSijoittele(Laskentakutsu laskentakutsu) {
        List<LaskeDTO> lista = laskentakutsu.getLaskeDTOs();
        String pollKey = laskentakutsu.getPollKey();

        LOG.info("Aloitetaan valintaryhmälaskenta uuid:lla {}", lista.get(0).getUuid());
        try {
            ValisijoitteluKasittelija.ValisijoiteltavatJonot valisijoiteltavatJonot = valisijoitteluKasittelija.valisijoiteltavatJonot(lista);
            if (valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
                lista.forEach(laskeDTO -> valintalaskentaService.laskeKaikki(laskeDTO.getHakemus(),
                    laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(), laskeDTO.getHakukohdeOid(), laskeDTO.getUuid(), laskeDTO.isKorkeakouluhaku()));
            } else {
                Map<Integer, List<LaskeDTO>> laskettavatHakukohteetVaiheittain = new TreeMap<>();
                lista.forEach(laskeDTO -> {
                    laskeDTO.getValintaperuste().forEach(v -> {
                        List<LaskeDTO> dtos = laskettavatHakukohteetVaiheittain.getOrDefault(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), new ArrayList<>());
                        dtos.add(new LaskeDTO(laskeDTO.getUuid(), laskeDTO.isKorkeakouluhaku(), laskeDTO.isErillishaku(), laskeDTO.getHakukohdeOid(), laskeDTO.getHakemus(), Arrays.asList(v), laskeDTO.getHakijaryhmat()));
                        laskettavatHakukohteetVaiheittain.put(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), dtos);
                    });
                });
                final int valinnanVaiheidenMaara = laskettavatHakukohteetVaiheittain.size();
                ValintakoelaskennanKumulatiivisetTulokset kumulatiivisetTulokset = new ValintakoelaskennanKumulatiivisetTulokset();
                laskettavatHakukohteetVaiheittain.keySet().stream().forEachOrdered(vaiheenJarjestysNumero -> {

                    List<LaskeDTO> laskePerValinnanvaihe = laskettavatHakukohteetVaiheittain.get(vaiheenJarjestysNumero);
                    final int hakukohteidenMaaraValinnanVaiheessa = laskePerValinnanvaihe.size();
                    for (int i = 0; i < laskePerValinnanvaihe.size(); ++i) { // indeksin vuoksi
                        LaskeDTO laskeDTO = laskePerValinnanvaihe.get(i);
                        final ValintaperusteetDTO valintaPerusteet = laskeDTO.getValintaperuste().get(0);
                        ValintaperusteetValinnanVaiheDTO valinnanVaihe = valintaPerusteet.getValinnanVaihe();
                        try {
                            if (valinnanVaihe.getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE)) {
                                LOG.info("(Uuid={}, {}={}/{}, hakukohde={}/{}) Suoritetaan valintakoelaskenta {} hakemukselle",
                                    laskeDTO.getUuid(),
                                    valinnanVaihe.getValinnanVaiheTyyppi(),
                                    vaiheenJarjestysNumero,
                                    valinnanVaiheidenMaara,
                                    i + 1,
                                    hakukohteidenMaaraValinnanVaiheessa,
                                    laskeDTO.getHakemus().size());
                                laskeDTO.getHakemus().forEach(h -> valintalaskentaService.valintakokeet(h, laskeDTO.getValintaperuste(), laskeDTO.getUuid(), kumulatiivisetTulokset, laskeDTO.isKorkeakouluhaku()));
                            } else {
                                boolean erillisHaku = isErillisHaku(laskeDTO, valintaPerusteet);

                                if (erillisHaku) {
                                    // Aseta sijoittelun käyttämät kentät
                                    setSijoittelunKayttamanKentat(valintaPerusteet);
                                }
                                LOG.info("(Uuid={}, {}={}/{}, hakukohde={}/{}) Suoritetaan laskenta {} hakemukselle",
                                    laskeDTO.getUuid(),
                                    valinnanVaihe.getValinnanVaiheTyyppi(),
                                    vaiheenJarjestysNumero,
                                    valinnanVaiheidenMaara,
                                    i + 1,
                                    hakukohteidenMaaraValinnanVaiheessa,
                                    laskeDTO.getHakemus().size());
                                valintalaskentaService.laske(laskeDTO.getHakemus(), laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(), laskeDTO.getHakukohdeOid(), laskeDTO.getUuid(), laskeDTO.isKorkeakouluhaku());
                                if (valisijoiteltavatJonot.valinnanvaiheet.contains(vaiheenJarjestysNumero)) {
                                    Map<String, List<String>> kohteet = valisijoiteltavatJonot.jonot;
                                    if (kohteet.containsKey(laskeDTO.getHakukohdeOid())) {
                                        List<String> jonot = kohteet.get(laskeDTO.getHakukohdeOid());
                                        LOG.info("(Uuid={}, {}={}/{}, hakukohde={}/{}) Suoritetaan välisijoittelu hakukohteelle {}",
                                            laskeDTO.getUuid(),
                                            valinnanVaihe.getValinnanVaiheTyyppi(),
                                            vaiheenJarjestysNumero,
                                            valinnanVaiheidenMaara,
                                            i + 1,
                                            hakukohteidenMaaraValinnanVaiheessa,
                                            laskeDTO.getHakukohdeOid());
                                        valisijoitteleKopiot(laskeDTO, haeKopiotValintaperusteista(jonot));
                                    }
                                }

                                if (erillisHaku) {
                                    erillisSijoittele(laskeDTO, valintaPerusteet);
                                }
                            }

                            LOG.info("(Uuid={}, {}={}/{}, hakukohde={}/{}) Laskenta suoritettu hakukohteessa {}",
                                laskeDTO.getUuid(),
                                valinnanVaihe.getValinnanVaiheTyyppi(),
                                vaiheenJarjestysNumero,
                                valinnanVaiheidenMaara,
                                i + 1,
                                hakukohteidenMaaraValinnanVaiheessa,
                                laskeDTO.getHakukohdeOid());
                        } catch (Throwable t) {
                            paivitaKohteenLaskennanTila(pollKey, HakukohteenLaskennanTila.VIRHE);
                            LOG.error("(Uuid={}, {}={}/{}, hakukohde={}/{}) virhe hakukohteelle {}",
                                laskeDTO.getUuid(),
                                valinnanVaihe.getValinnanVaiheTyyppi(),
                                vaiheenJarjestysNumero,
                                valinnanVaiheidenMaara,
                                i + 1,
                                hakukohteidenMaaraValinnanVaiheessa,
                                laskeDTO.getHakukohdeOid()
                                , t);
                            throw new RuntimeException(t);

                        }
                    }

                });
            }
            paivitaKohteenLaskennanTila(pollKey, HakukohteenLaskennanTila.VALMIS);
        } catch (Throwable t) {
            LOG.error("Valintaryhmälaskennassa tapahtui yllättävä virhe. Lopetetaan ja merkitään laskenta virheelliseksi. ", t);
            paivitaKohteenLaskennanTila(pollKey, HakukohteenLaskennanTila.VIRHE);
        }
    }

    private void setSijoittelunKayttamanKentat(ValintaperusteetDTO v) {
        v.getValinnanVaihe().getValintatapajono().forEach(j -> {
            j.setSiirretaanSijoitteluun(true);
            j.setValmisSijoiteltavaksi(true);
        });
    }

    private BiFunction<String, Runnable, Runnable> timeRunnable = (uuid, r) -> {
        return () -> {
            long start = System.currentTimeMillis();
            try {
                r.run();
            } finally {
                long end = System.currentTimeMillis();
                LOG.info("(Uuid={}) (Kesto {}s) Laskenta valmis!", uuid, millisToString(end - start));
            }
        };
    };

    private static String millisToString(long millis) {
        return new BigDecimal(millis).divide(new BigDecimal(1000), 2, BigDecimal.ROUND_HALF_UP).toPlainString();
    }
}
