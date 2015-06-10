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
    @Autowired
    private ValintalaskentaService valintalaskentaService;

    @Autowired
    private ValisijoitteluKasittelija valisijoitteluKasittelija;

    @Autowired
    private ValiSijoitteluResource valiSijoitteluResource;

    @Autowired
    private ErillisSijoitteluResource erillisSijoitteluResource;

    @Autowired
    private ValintaperusteetValintatapajonoResource valintatapajonoResource;


    @Path("laske")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laske(LaskeDTO laskeDTO) {
        LOG.info("(Uuid={}) Aloitetaan laskenta hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
        ValisijoitteluKasittelija.ValisijoiteltavatJonot valisijoiteltavatJonot = valisijoitteluKasittelija.valisijoiteltavatJonot(Arrays.asList(laskeDTO));
        if (!valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
            valisijoiteltavatJonot = new ValisijoitteluKasittelija.ValisijoiteltavatJonot(valisijoiteltavatJonot.valinnanvaiheet, haeKopiotValintaperusteista(valisijoiteltavatJonot.jonot.get(laskeDTO.getHakukohdeOid())));
        }
        try {
            ValintaperusteetDTO valintaperusteetDTO = laskeDTO.getValintaperuste().get(0);
            boolean erillisHaku = laskeDTO.isErillishaku()
                    && valintaperusteetDTO.getViimeinenValinnanvaihe()
                    == valintaperusteetDTO.getValinnanVaihe().getValinnanVaiheJarjestysluku();

            if (erillisHaku) {
                setSijoittelunKayttamanKentat(valintaperusteetDTO);
            }
            LOG.info("(Uuid={}) Suoritetaan laskenta. Hakemuksia {} kpl ja valintaperusteita {} kpl",
                    laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getValintaperuste().size());
            valintalaskentaService.laske(laskeDTO.getHakemus(),
                    laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(),
                    laskeDTO.getHakukohdeOid(), laskeDTO.getUuid());
            if (erillisHaku) {
                List<String> jonot = valintaperusteetDTO.getValinnanVaihe().getValintatapajono().stream().map(j -> j.getOid()).collect(Collectors.toList());
                Map<String, List<String>> map = new HashMap<>();
                map.put(valintaperusteetDTO.getHakukohdeOid(), jonot);
                erillissijoitteleJonot(laskeDTO, map);
            }
        } catch (Exception e) {
            LOG.error("(Uuid={}) Valintalaskenta epaonnistui: {}\r\n{}", laskeDTO.getUuid(), e.getMessage(), Arrays.toString(e.getStackTrace()));
            throw e;
        }
        if (!valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
            valisijoitteleKopiot(laskeDTO, valisijoiteltavatJonot.jonot);
        }

        LOG.info("(Uuid={}) Laskenta suoritettu hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());

        return "Onnistui!";
    }

    @Path("valintakokeet")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String valintakokeet(LaskeDTO laskeDTO) {
        try {
            LOG.info("(Uuid={}) Suoritetaan valintakoelaskenta {} hakemukselle hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getHakukohdeOid());
            laskeDTO.getHakemus().forEach(h -> valintalaskentaService.valintakokeet(h, laskeDTO.getValintaperuste(), laskeDTO.getUuid()));
            LOG.info("(Uuid={}) Valintakoelaskenta suoritettu {} hakemukselle hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getHakukohdeOid());
            return "Onnistui";
        } catch (Exception e) {
            LOG.error("(Uuid={}) Valintakoelaskenta epaonnistui: {}\r\n{}", laskeDTO.getUuid(), e.getMessage(), Arrays.toString(e.getStackTrace()));
            throw e;
        }
    }

    @Path("laskekaikki")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laskeKaikki(LaskeDTO laskeDTO) {
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
                        laskeDTO.getHakukohdeOid(), laskeDTO.getUuid());
                if (laskeDTO.isErillishaku()) {
                    laskeDTO.getValintaperuste().stream()
                            .filter(v -> v.getValinnanVaihe().getValinnanVaiheJarjestysluku() == v.getViimeinenValinnanvaihe())
                            .forEach(v -> {
                                List<String> jonot = v.getValinnanVaihe().getValintatapajono().stream().map(j -> j.getOid()).collect(Collectors.toList());
                                Map<String, List<String>> map = new HashMap<>();
                                map.put(v.getHakukohdeOid(), jonot);
                                erillissijoitteleJonot(laskeDTO, map);
                            });
                }
            } else {
                Map<Integer, List<LaskeDTO>> map = new TreeMap<>();
                laskeDTO.getValintaperuste().forEach(v -> {
                    List<LaskeDTO> dtos = map.getOrDefault(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), new ArrayList<>());
                    dtos.add(new LaskeDTO(laskeDTO.getUuid(), laskeDTO.isErillishaku(), laskeDTO.getHakukohdeOid(), laskeDTO.getHakemus(), Arrays.asList(v), laskeDTO.getHakijaryhmat()));
                    map.put(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), dtos);
                });

                map.keySet().stream().forEachOrdered(key -> {
                    map.get(key).forEach(dto -> {
                        ValintaperusteetValinnanVaiheDTO valinnanVaihe = dto.getValintaperuste().get(0).getValinnanVaihe();
                        if (valinnanVaihe.getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE)) {
                            LOG.info("(Uuid={}) Suoritetaan valintakoelaskenta {} hakemukselle", laskeDTO.getUuid(), laskeDTO.getHakemus().size());
                            laskeDTO.getHakemus().forEach(h -> valintalaskentaService.valintakokeet(h, dto.getValintaperuste(), laskeDTO.getUuid()));
                        } else {
                            ValintaperusteetDTO valintaperusteetDTO = dto.getValintaperuste().get(0);
                            boolean erillisHaku = laskeDTO.isErillishaku()
                                    && valintaperusteetDTO.getViimeinenValinnanvaihe()
                                    == valintaperusteetDTO.getValinnanVaihe().getValinnanVaiheJarjestysluku();

                            if (erillisHaku) {
                                // Aseta sijoittelun käyttämät kentät
                                setSijoittelunKayttamanKentat(valintaperusteetDTO);
                            }
                            LOG.info("(Uuid={}) Suoritetaan laskenta. Hakemuksia {} kpl ja valintaperusteita {} kpl",
                                    laskeDTO.getUuid(), laskeDTO.getHakemus().size(), laskeDTO.getValintaperuste().size());
                            valintalaskentaService.laske(dto.getHakemus(), dto.getValintaperuste(), dto.getHakijaryhmat(), dto.getHakukohdeOid(), laskeDTO.getUuid());

                            if (erillisHaku) {
                                List<String> jonot = valintaperusteetDTO.getValinnanVaihe().getValintatapajono().stream().map(j -> j.getOid()).collect(Collectors.toList());
                                Map<String, List<String>> kohteet = new HashMap<>();
                                kohteet.put(valintaperusteetDTO.getHakukohdeOid(), jonot);
                                erillissijoitteleJonot(dto, kohteet);
                            }
                        }
                    });
                    if (valisijoiteltavatJonot.valinnanvaiheet.contains(key)) {
                        valisijoitteleKopiot(laskeDTO, new ImmutablePair<>(valisijoiteltavatJonot.valinnanvaiheet, haeKopiotValintaperusteista(valisijoiteltavatJonot.jonot.get(laskeDTO.getHakukohdeOid()))).getRight());
                    }
                });
            }
        } catch (Exception e) {
            LOG.error("(Uuid={}) Valintalaskenta ja valintakoelaskenta epaonnistui: {}\r\n{}",
                    laskeDTO.getUuid(), e.getMessage(), Arrays.toString(e.getStackTrace()));
            throw e;
        }

        LOG.info("(Uuid={}) Laskenta suoritettu hakukohteessa {}", laskeDTO.getUuid(), laskeDTO.getHakukohdeOid());
        return "Onnistui!";
    }

    private void setSijoittelunKayttamanKentat(ValintaperusteetDTO v) {
        v.getValinnanVaihe().getValintatapajono().forEach(j -> {
            j.setSiirretaanSijoitteluun(true);
            j.setValmisSijoiteltavaksi(true);
        });
    }

    @Path("laskejasijoittele")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laskeJaSijoittele(List<LaskeDTO> lista) {
        ValisijoitteluKasittelija.ValisijoiteltavatJonot valisijoiteltavatJonot = valisijoitteluKasittelija.valisijoiteltavatJonot(lista);
        if (valisijoiteltavatJonot.valinnanvaiheet.isEmpty()) {
            lista.forEach(laskeDTO -> valintalaskentaService.laskeKaikki(laskeDTO.getHakemus(),
                    laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(), laskeDTO.getHakukohdeOid(), laskeDTO.getUuid()));
        } else {
            Map<Integer, List<LaskeDTO>> laskettavatHakukohteetVaiheittain = new TreeMap<>();
            lista.forEach(laskeDTO -> {
                laskeDTO.getValintaperuste().forEach(v -> {
                    List<LaskeDTO> dtos = laskettavatHakukohteetVaiheittain.getOrDefault(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), new ArrayList<>());
                    dtos.add(new LaskeDTO(laskeDTO.getUuid(), laskeDTO.isErillishaku(), laskeDTO.getHakukohdeOid(), laskeDTO.getHakemus(), Arrays.asList(v), laskeDTO.getHakijaryhmat()));
                    laskettavatHakukohteetVaiheittain.put(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), dtos);
                });
            });
            final int valinnanVaiheidenMaara = laskettavatHakukohteetVaiheittain.size();
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
                            laskeDTO.getHakemus().forEach(h -> valintalaskentaService.valintakokeet(h, laskeDTO.getValintaperuste(), laskeDTO.getUuid()));
                        } else {
                            ValintaperusteetDTO valintaperusteetDTO = valintaPerusteet;
                            boolean erillisHaku = laskeDTO.isErillishaku()
                                    && valintaperusteetDTO.getViimeinenValinnanvaihe()
                                    == valintaperusteetDTO.getValinnanVaihe().getValinnanVaiheJarjestysluku();

                            if (erillisHaku) {
                                // Aseta sijoittelun käyttämät kentät
                                setSijoittelunKayttamanKentat(valintaperusteetDTO);
                            }
                            LOG.info("(Uuid={}, {}={}/{}, hakukohde={}/{}) Suoritetaan laskenta {} hakemukselle",
                                    laskeDTO.getUuid(),
                                    valinnanVaihe.getValinnanVaiheTyyppi(),
                                    vaiheenJarjestysNumero,
                                    valinnanVaiheidenMaara,
                                    i+1,
                                    hakukohteidenMaaraValinnanVaiheessa,
                                    laskeDTO.getHakemus().size());
                            valintalaskentaService.laske(laskeDTO.getHakemus(), laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(), laskeDTO.getHakukohdeOid(), laskeDTO.getUuid());
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
                                List<String> jonot = valintaperusteetDTO.getValinnanVaihe().getValintatapajono().stream().map(j -> j.getOid()).collect(Collectors.toList());
                                Map<String, List<String>> kohteet = new HashMap<>();
                                kohteet.put(valintaperusteetDTO.getHakukohdeOid(), jonot);
                                erillissijoitteleJonot(laskeDTO, kohteet);
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

        return "Onnistui";
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
            LOG.error("(Uuid={}) Välisijoittelulle ei löytynyt hakuoidia!", laskeDTO.getUuid(), e.getMessage(), Arrays.toString(e.getStackTrace()));
            throw e;
        }
        return hakuoid;
    }

    private void erillissijoitteleJonot(LaskeDTO laskeDTO, Map<String, List<String>> jonot) {
        String hakuoid;
        try {
            hakuoid = laskeDTO.getValintaperuste().get(0).getHakuOid();
        } catch (Exception e) {
            LOG.error("(Uuid={}) Erillissijoittelulle ei löytynyt hakuoidia!",
                    laskeDTO.getUuid(), e.getMessage(), Arrays.toString(e.getStackTrace()));
            throw e;
        }
        ValisijoitteluDTO dto = new ValisijoitteluDTO();
        dto.setHakukohteet(jonot);
        Long ajo = erillisSijoitteluResource.sijoittele(hakuoid, dto);

        valintalaskentaService.applyErillissijoittelu(jonot, ajo);
    }
}
