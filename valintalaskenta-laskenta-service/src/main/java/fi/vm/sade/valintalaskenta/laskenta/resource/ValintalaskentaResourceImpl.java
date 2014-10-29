package fi.vm.sade.valintalaskenta.laskenta.resource;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import com.google.gson.GsonBuilder;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetValinnanVaiheDTO;
import fi.vm.sade.service.valintaperusteet.dto.model.ValinnanVaiheTyyppi;
import fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO;
import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.resource.ValintalaskentaResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.SijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;

import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

/**
 * Created by jukais on 21.3.2014.
 */
@Component
@Path("valintalaskenta")
public class ValintalaskentaResourceImpl implements ValintalaskentaResource {
	private static final Logger LOG = LoggerFactory
			.getLogger(ValintalaskentaResourceImpl.class);
	@Autowired
	private ValintalaskentaService valintalaskentaService;

    @Autowired
    private ValisijoitteluKasittelija valisijoitteluKasittelija;

    @Autowired
    private SijoitteluResource sijoitteluResource;

    @Autowired
    private ValintaperusteetValintatapajonoResource valintatapajonoResource;


	@Override
	@Path("laske")
	@Consumes("application/json")
	@Produces("text/plain")
	@POST
	public String laske(LaskeDTO laskeDTO) {
        //System.out.println(new GsonBuilder().create().toJson(laskeDTO));
        Pair<Set<Integer>, Map<String, List<String>>> valisijoiteltavatJonot = valisijoitteluKasittelija.valisijoiteltavatJonot(Arrays.asList(laskeDTO));
        if(!valisijoiteltavatJonot.getLeft().isEmpty()) {
            valisijoiteltavatJonot = new ImmutablePair<>(valisijoiteltavatJonot.getLeft(), haeKopiotValintaperusteista(valisijoiteltavatJonot.getRight().get(laskeDTO.getHakukohdeOid())));
        }
		try {
			valintalaskentaService.laske(laskeDTO.getHakemus(),
					laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(),
					laskeDTO.getHakukohdeOid());
		} catch (Exception e) {
			LOG.error("Valintalaskenta epaonnistui: {}\r\n{}", e.getMessage(),
					Arrays.toString(e.getStackTrace()));
			throw e;
		}
        if(!valisijoiteltavatJonot.getLeft().isEmpty()) {
            valisijoitteleKopiot(laskeDTO, valisijoiteltavatJonot.getRight());
        }
        return "Onnistui!";
	}

	@Override
	@Path("valintakokeet")
	@Consumes("application/json")
	@Produces("text/plain")
	@POST
	public String valintakokeet(LaskeDTO laskeDTO) {
        //System.out.println(new GsonBuilder().create().toJson(laskeDTO));
		try {
            LOG.error("Suoritetaan valintakoelaskenta {} hakemukselle", laskeDTO.getHakemus().size());
			laskeDTO.getHakemus()
					.forEach(
							h -> valintalaskentaService.valintakokeet(h,
									laskeDTO.getValintaperuste()));
			return "Onnistui";
		} catch (Exception e) {
			LOG.error("Valintakoelaskenta epaonnistui: {}\r\n{}",
					e.getMessage(), Arrays.toString(e.getStackTrace()));
			throw e;
		}
	}

	@Override
	@Path("laskekaikki")
	@Consumes("application/json")
	@Produces("text/plain")
	@POST
	public String laskeKaikki(LaskeDTO laskeDTO) {
        //System.out.println(new GsonBuilder().create().toJson(laskeDTO));
        Pair<Set<Integer>, Map<String, List<String>>> valisijoiteltavatJonot = valisijoitteluKasittelija.valisijoiteltavatJonot(Arrays.asList(laskeDTO));

		try {
            if(valisijoiteltavatJonot.getLeft().isEmpty()) {
                valintalaskentaService.laskeKaikki(laskeDTO.getHakemus(),
                        laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(),
                        laskeDTO.getHakukohdeOid());
            } else {
                Map<Integer, List<LaskeDTO>> map = new TreeMap<>();
                laskeDTO.getValintaperuste().forEach(v -> {
                    List<LaskeDTO> dtos = map.getOrDefault(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), new ArrayList<>());
                    dtos.add(new LaskeDTO(laskeDTO.isErillishaku(), laskeDTO.getHakukohdeOid(), laskeDTO.getHakemus(), Arrays.asList(v), laskeDTO.getHakijaryhmat()));
                    map.put(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), dtos);
                });

                map.keySet().stream().forEachOrdered(key -> {
                    map.get(key).forEach(dto -> {
                        ValintaperusteetValinnanVaiheDTO valinnanVaihe = dto.getValintaperuste().get(0).getValinnanVaihe();
                        if(valinnanVaihe.getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE)) {
                            LOG.error("Suoritetaan valintakoelaskenta {} hakemukselle", laskeDTO.getHakemus().size());
                            laskeDTO.getHakemus().forEach(h -> valintalaskentaService.valintakokeet(h, dto.getValintaperuste()));
                        } else {
                            valintalaskentaService.laske(dto.getHakemus(), dto.getValintaperuste(), dto.getHakijaryhmat(), dto.getHakukohdeOid());

                        }
                    });
                    if(valisijoiteltavatJonot.getLeft().contains(key)) {
                        valisijoitteleKopiot(laskeDTO, new ImmutablePair<>(valisijoiteltavatJonot.getLeft(), haeKopiotValintaperusteista(valisijoiteltavatJonot.getRight().get(laskeDTO.getHakukohdeOid()))).getRight());
                    }
                });
            }
		} catch (Exception e) {
			LOG.error(
					"Valintalaskenta ja valintakoelaskenta epaonnistui: {}\r\n{}",
					e.getMessage(), Arrays.toString(e.getStackTrace()));
			throw e;
		}

        return "Onnistui!";
	}

    @Override
    @Path("laskejasijoittele")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laskeJaSijoittele(List<LaskeDTO> lista) {
        Pair<Set<Integer>, Map<String, List<String>>> valisijoiteltavatJonot = valisijoitteluKasittelija.valisijoiteltavatJonot(lista);

        if(valisijoiteltavatJonot.getLeft().isEmpty()) {
            lista.forEach(laskeDTO -> valintalaskentaService.laskeKaikki(laskeDTO.getHakemus(),
                    laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(), laskeDTO.getHakukohdeOid()));
        } else {
            Map<Integer, List<LaskeDTO>> map = new TreeMap<>();
            lista.forEach(laskeDTO -> {
                laskeDTO.getValintaperuste().forEach(v -> {
                    List<LaskeDTO> dtos = map.getOrDefault(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), new ArrayList<>());
                    dtos.add(new LaskeDTO(laskeDTO.isErillishaku(),laskeDTO.getHakukohdeOid(), laskeDTO.getHakemus(), Arrays.asList(v), laskeDTO.getHakijaryhmat()));
                    map.put(v.getValinnanVaihe().getValinnanVaiheJarjestysluku(), dtos);
                });
            });

            map.keySet().stream().forEachOrdered(key -> {
                map.get(key).forEach(laskeDTO -> {
                    ValintaperusteetValinnanVaiheDTO valinnanVaihe = laskeDTO.getValintaperuste().get(0).getValinnanVaihe();
                    if(valinnanVaihe.getValinnanVaiheTyyppi().equals(ValinnanVaiheTyyppi.VALINTAKOE)) {
                        LOG.error("Suoritetaan valintakoelaskenta {} hakemukselle", laskeDTO.getHakemus().size());
                        laskeDTO.getHakemus().forEach(h -> valintalaskentaService.valintakokeet(h, laskeDTO.getValintaperuste()));
                    } else {
                       valintalaskentaService.laske(laskeDTO.getHakemus(), laskeDTO.getValintaperuste(), laskeDTO.getHakijaryhmat(), laskeDTO.getHakukohdeOid());
                        if(valisijoiteltavatJonot.getLeft().contains(key)) {
                            Map<String, List<String>> kohteet = valisijoiteltavatJonot.getRight();
                            if(kohteet.containsKey(laskeDTO.getHakukohdeOid())) {
                                List<String> jonot = kohteet.get(laskeDTO.getHakukohdeOid());
                                valisijoitteleKopiot(laskeDTO, haeKopiotValintaperusteista(jonot));
                            }

                        }
                    }
                });

            });
        }

        return "Onnistui";
    }

    private Map<String, List<String>> haeKopiotValintaperusteista(List<String> jonot) {
        return valintatapajonoResource.findKopiot(jonot);
    }

    private void valisijoitteleKopiot(LaskeDTO laskeDTO, Map<String, List<String>> valisijoiteltavatJonot) {
        String hakuoid;
        try {
            hakuoid = laskeDTO.getValintaperuste().get(0).getHakuOid();
        } catch (Exception e) {
            LOG.error(
                    "Välisijoittelulle ei löytynyt hakuoidia!",
                    e.getMessage(), Arrays.toString(e.getStackTrace()));
            throw e;
        }
        ValisijoitteluDTO dto = new ValisijoitteluDTO();
        dto.setHakukohteet(valisijoiteltavatJonot);
        List<HakukohdeDTO> sijoitellut = sijoitteluResource.sijoittele(hakuoid, dto);

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

}
