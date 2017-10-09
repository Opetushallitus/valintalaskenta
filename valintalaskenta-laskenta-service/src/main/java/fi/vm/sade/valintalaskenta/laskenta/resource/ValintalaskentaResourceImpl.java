package fi.vm.sade.valintalaskenta.laskenta.resource;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO;
import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import fi.vm.sade.valintalaskenta.domain.HakukohteenLaskennanTila;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
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

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

@Controller
@Path("valintalaskenta")
public class ValintalaskentaResourceImpl {
    private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaResourceImpl.class);

    private final ValintalaskentaService valintalaskentaService;
    private final ValisijoitteluKasittelija valisijoitteluKasittelija;
    private final ValiSijoitteluResource valiSijoitteluResource;
    private final ErillisSijoitteluResource erillisSijoitteluResource;
    private final ValintaperusteetValintatapajonoResource valintatapajonoResource;

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

        this.hakukohteetLaskettavina = new ConcurrentHashMap<>();
    }

    @Path("laske")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laske(LaskeDTO laskeDTO) throws Exception {

        String status = pidaKirjaaMeneillaanOlevista(laskeDTO);
        if(!status.equals(HakukohteenLaskennanTila.UUSI)) {
            return status;
        } else {
            //Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran (samalle ooid+haukohdeoid-tunnisteelle) vaikka pyyntöjä tulisi useita
            try {
                Runnable kaynnistaLaskenta = () -> toteutaLaskenta(laskeDTO);
                Thread laskenta = new Thread(kaynnistaLaskenta);
                laskenta.start();
                return HakukohteenLaskennanTila.UUSI;
            } catch (Exception e) {
                LOG.error("Virhe laskennan suorituksessa, ", e);
                throw e;
            }
        }
    }

    @Path("valintakokeet")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String valintakokeet(LaskeDTO laskeDTO) throws Exception {

        String status = pidaKirjaaMeneillaanOlevista(laskeDTO);
        if(!status.equals(HakukohteenLaskennanTila.UUSI)) {
            return status;
        } else {
            //Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran (samalle ooid+haukohdeoid-tunnisteelle) vaikka pyyntöjä tulisi useita
            try {
                Runnable kaynnistaLaskenta = () -> toteutaValintakoeLaskenta(laskeDTO);
                Thread laskenta = new Thread(kaynnistaLaskenta);
                laskenta.start();
                return HakukohteenLaskennanTila.UUSI;
            } catch (Exception e) {
                LOG.error("Virhe laskennan suorituksessa, ", e);
                throw e;
            }
        }
    }

    @Path("laskekaikki")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laskeKaikki(final LaskeDTO laskeDTO) throws Exception {

        String status = pidaKirjaaMeneillaanOlevista(laskeDTO);
        if(!status.equals(HakukohteenLaskennanTila.UUSI)) {
            return status;
        } else {
            //Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran (samalle ooid+haukohdeoid-tunnisteelle) vaikka pyyntöjä tulisi useita
            try {
                Runnable kaynnistaLaskenta = () -> toteutaLaskeKaikki(laskeDTO);
                Thread laskenta = new Thread(kaynnistaLaskenta);
                laskenta.start();
                return HakukohteenLaskennanTila.UUSI;
            } catch (Exception e) {
                LOG.error("Virhe laskennan suorituksessa, ", e);
                throw e;
            }
        }
    }

    @Path("laskejasijoittele")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laskeJaSijoittele(List<LaskeDTO> lista) {

        LaskeDTO laskeDTO;
        if(!lista.isEmpty()) {
            laskeDTO = lista.get(0);
        } else {
            return HakukohteenLaskennanTila.VIRHE;
        }
        String status = pidaKirjaaMeneillaanOlevista(laskeDTO);
        if(!status.equals(HakukohteenLaskennanTila.UUSI)) {
            return status;
        } else {
            //Luodaan uusi laskentatoteutus hakukohteelle, tämä käynnistetään vain kerran (samalle ooid+haukohdeoid-tunnisteelle) vaikka pyyntöjä tulisi useita
            try {
                Runnable kaynnistaLaskenta = () -> toteutaLaskeJaSijoittele(lista);
                Thread laskenta = new Thread(kaynnistaLaskenta);
                laskenta.start();
                return HakukohteenLaskennanTila.UUSI;
            } catch (Exception e) {
                LOG.error("Virhe laskennan suorituksessa, ", e);
                throw e;
            }
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

    private String pidaKirjaaMeneillaanOlevista(LaskeDTO laskeDTO) {
        String key = laskeDTO.getUuid()+laskeDTO.getHakukohdeOid();
        if (!hakukohteetLaskettavina.containsKey(key)) {
            LOG.info("Luodaan uusi laskettava hakukohde. {} {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
            hakukohteetLaskettavina.put(key, HakukohteenLaskennanTila.KESKEN);
            return HakukohteenLaskennanTila.UUSI;
        } else if (hakukohteetLaskettavina.get(key).equals(HakukohteenLaskennanTila.VALMIS)) {
            LOG.info("Kohteen laskenta on valmistunut. {} {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
            //hakukohteetLaskettavina.remove(key); //!! Joissain tilanteissa käynnistetään tarpeettomasti uusi laskenta
            return HakukohteenLaskennanTila.VALMIS;
        } else if (hakukohteetLaskettavina.get(key).equals(HakukohteenLaskennanTila.VIRHE)) {
            LOG.info("Kohteen laskennassa on tapahtunut virhe. {} {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
            return HakukohteenLaskennanTila.VIRHE;
        } else {
            LOG.info("Hakukohteen laskenta on edelleen kesken. {} {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
            return HakukohteenLaskennanTila.KESKEN;
        }
    }

    private void paivitaKohteenLaskennanTila(LaskeDTO laskeDTO, String tila) {
        if(tila.equals(HakukohteenLaskennanTila.VALMIS)) {
            LOG.info("Ollaan valmiita, merkataan kohteen laskennan tila valmiiksi. {} {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
        }
        String key = laskeDTO.getUuid()+laskeDTO.getHakukohdeOid();
        if(hakukohteetLaskettavina.containsKey(key)) {
            hakukohteetLaskettavina.replace(key, tila);
        }
    }

    private boolean isErillisHaku(LaskeDTO laskeDTO, ValintaperusteetDTO valintaperusteetDTO) {
        return laskeDTO.isErillishaku()
                && valintaperusteetDTO.getViimeinenValinnanvaihe()
                == valintaperusteetDTO.getValinnanVaihe().getValinnanVaiheJarjestysluku();
    }

    private void toteutaLaskenta(LaskeDTO laskeDTO) {
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
        } catch (Exception e) {
            LOG.error("Valintalaskenta epaonnistui! uuid=" + laskeDTO.getUuid(), e);
            paivitaKohteenLaskennanTila(laskeDTO, HakukohteenLaskennanTila.VIRHE);
        }
        if (!valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
            valisijoitteleKopiot(laskeDTO, valisijoiteltavatJonot.jonot);
        }

        LOG.info("(Uuid={}) Laskenta suoritettu hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
        paivitaKohteenLaskennanTila(laskeDTO, HakukohteenLaskennanTila.VALMIS);
    }

    private void toteutaValintakoeLaskenta(LaskeDTO laskeDTO) {
        try {
            LOG.info("(Uuid={}) Suoritetaan valintakoelaskenta {} hakemukselle hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getHakukohdeOid());
            laskeDTO.getHakemus().forEach(h -> valintalaskentaService.valintakokeet(h, laskeDTO.getValintaperuste(), laskeDTO.getUuid(), new ValintakoelaskennanKumulatiivisetTulokset(), laskeDTO.isKorkeakouluhaku()));
            LOG.info("(Uuid={}) Valintakoelaskenta suoritettu {} hakemukselle hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getHakukohdeOid());
            paivitaKohteenLaskennanTila(laskeDTO, HakukohteenLaskennanTila.VALMIS);
        } catch (Exception e) {
            LOG.error("Valintakoelaskenta epaonnistui! uuid=" + laskeDTO.getUuid(), e);
            paivitaKohteenLaskennanTila(laskeDTO, HakukohteenLaskennanTila.VIRHE);
        }
    }

    public void toteutaLaskeKaikki(LaskeDTO laskeDTO) {
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
        } catch (Exception e) {
            LOG.error("Valintalaskenta ja valintakoelaskenta epaonnistui! uuid=" + laskeDTO.getUuid(), e);
            paivitaKohteenLaskennanTila(laskeDTO, HakukohteenLaskennanTila.VIRHE);
        }

        LOG.info("(Uuid={}) Laskenta suoritettu hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
        paivitaKohteenLaskennanTila(laskeDTO, HakukohteenLaskennanTila.VALMIS);
    }

    private void toteutaLaskeJaSijoittele(List<LaskeDTO> lista) {

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
                                    i+1,
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
                                    i+1,
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
                                            i+1,
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
                                i+1,
                                hakukohteidenMaaraValinnanVaiheessa,
                                laskeDTO.getHakukohdeOid());
                    } catch (Throwable t) {
                        paivitaKohteenLaskennanTila(lista.get(0), HakukohteenLaskennanTila.VIRHE);
                        LOG.error("(Uuid={}, {}={}/{}, hakukohde={}/{}) virhe hakukohteelle {}",
                                laskeDTO.getUuid(),
                                valinnanVaihe.getValinnanVaiheTyyppi(),
                                vaiheenJarjestysNumero,
                                valinnanVaiheidenMaara,
                                i+1,
                                hakukohteidenMaaraValinnanVaiheessa,
                                laskeDTO.getHakukohdeOid()
                                , t);
                        throw new RuntimeException(t);

                    }
                }

            });
        }
        paivitaKohteenLaskennanTila(lista.get(0), HakukohteenLaskennanTila.VALMIS);
    }

    private void setSijoittelunKayttamanKentat(ValintaperusteetDTO v) {
        v.getValinnanVaihe().getValintatapajono().forEach(j -> {
            j.setSiirretaanSijoitteluun(true);
            j.setValmisSijoiteltavaksi(true);
        });
    }
}
