package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.jws.WebParam;
import javax.xml.datatype.DatatypeFactory;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.ConversionService;

import com.google.common.collect.Lists;

import fi.vm.sade.service.valintaperusteet.schema.TasasijasaantoTyyppi;
import fi.vm.sade.service.valintatiedot.ValintatietoService;
import fi.vm.sade.service.valintatiedot.schema.AvainArvoTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakemusOsallistuminenTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakemusTilaTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakijaTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakuTyyppi;
import fi.vm.sade.service.valintatiedot.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintatiedot.schema.Osallistuminen;
import fi.vm.sade.service.valintatiedot.schema.SyotettyArvoTyyppi;
import fi.vm.sade.service.valintatiedot.schema.ValinnanvaiheTyyppi;
import fi.vm.sade.service.valintatiedot.schema.ValintakoeOsallistuminenTyyppi;
import fi.vm.sade.service.valintatiedot.schema.ValintatapajonoTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.SyotettyArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 * User: kkammone Date: 29.4.2013 Time: 13:24
 */
// @PreAuthorize("isAuthenticated()")
public class ValintatietoServiceImpl implements ValintatietoService {

	private static final Logger LOG = LoggerFactory
			.getLogger(ValintatietoServiceImpl.class);
	@Autowired
	private ValintalaskentaTulosService tulosService;

	@Autowired
	private ConversionService conversionService;

	@Override
	// @PreAuthorize(READ_UPDATE_CRUD)
	public List<HakemusOsallistuminenTyyppi> haeValintatiedotHakukohteelle(
			@WebParam(name = "valintakoeOid", targetNamespace = "") List<String> valintakoeOid,
			@WebParam(name = "hakukohdeOid", targetNamespace = "") String hakukohdeOid) {
		List<HakemusOsallistuminenTyyppi> osallistumiset = new ArrayList<HakemusOsallistuminenTyyppi>();
		try {
			List<ValintakoeOsallistuminen> valinnanvaiheet = tulosService
					.haeValintakoeOsallistumisetByHakutoive(hakukohdeOid);
			Set<String> oidit = new HashSet<String>(valintakoeOid);
			GregorianCalendar kalenteri = new GregorianCalendar();
			for (ValintakoeOsallistuminen koetulos : valinnanvaiheet) {
				for (Hakutoive hakutoive : koetulos.getHakutoiveet()) {
					if (!hakukohdeOid.equals(hakutoive.getHakukohdeOid())) {
						// vain hakukohteeseen liittyvat valintakokeet mukaan
						// tulokseen
						// samoja valintakoeoideja kaytetaan myos muissa
						// hakukohteissa
						continue;
					}
					for (ValintakoeValinnanvaihe vaihe : hakutoive
							.getValinnanVaiheet()) {
						HakemusOsallistuminenTyyppi h = new HakemusOsallistuminenTyyppi();
						for (Valintakoe valintakoe : vaihe.getValintakokeet()) {
							if (oidit.contains(valintakoe.getValintakoeOid())) {
								ValintakoeOsallistuminenTyyppi osallistuminen = new ValintakoeOsallistuminenTyyppi();
								osallistuminen.setOsallistuminen(Osallistuminen
										.valueOf(valintakoe
												.getOsallistuminenTulos()
												.getOsallistuminen().name()));
								osallistuminen.setValintakoeOid(valintakoe
										.getValintakoeOid());
								osallistuminen.setValintakoeTunniste(valintakoe
										.getValintakoeTunniste());
								osallistuminen.setNimi(valintakoe.getNimi());
								h.getOsallistumiset().add(osallistuminen);
							}
						}
						// lisataan tulosjoukkoon vaan jos valinnanvaiheessa oli
						// valintakoe hakemukselle!
						if (!h.getOsallistumiset().isEmpty()) {
							kalenteri.setTime(koetulos.getCreatedAt());
							try {
								h.setLuontiPvm(DatatypeFactory.newInstance()
										.newXMLGregorianCalendar(kalenteri));
							} catch (Exception e) {
								e.printStackTrace(); // <- creating date failed!
							}
							h.setEtunimi(koetulos.getEtunimi());
							h.setSukunimi(koetulos.getSukunimi());
							h.setHakemusOid(koetulos.getHakemusOid());
							osallistumiset.add(h);
						}
					}
				}
			}
		} catch (Exception e) {
			LOG.error("Valintakoelaskennan osallitujia ei saatu haettua!");
			LOG.error("Virhe osallistujien hakemisessa: {} {} {}",
					e.getMessage(), e.getCause(),
					Arrays.toString(e.getStackTrace()));
			throw new RuntimeException(
					"Valintatieto osallistujille pyyntö epäonnistui!", e);
		}
		return osallistumiset;
	}

	@Override
	// @PreAuthorize(READ_UPDATE_CRUD)
	public HakuTyyppi haeValintatiedot(
			@WebParam(name = "hakuOid", targetNamespace = "") String hakuOid) {
		try {
			List<HakukohdeDTO> a = tulosService
					.haeLasketutValinnanvaiheetHaulle(hakuOid);

			HakuTyyppi hakuTyyppi = new HakuTyyppi();
			hakuTyyppi.setHakuOid(hakuOid);

			for (HakukohdeDTO v : a) {
				HakukohdeTyyppi ht = new HakukohdeTyyppi();
				ht.setOid(v.getOid());
				ht.setTarjoajaOid(v.getTarjoajaoid());
				hakuTyyppi.getHakukohteet().add(ht);

				for (ValinnanvaiheDTO valinnanvaiheDTO : v.getValinnanvaihe()) {
					ht.getValinnanvaihe().add(
							createValinnanvaiheTyyppi(valinnanvaiheDTO));

				}
			}
			return hakuTyyppi;
		} catch (Exception e) {
			LOG.error("Valintatietoja ei saatu haettua!");
			LOG.error("Virhe valintatietojen hakemisessa: {} {} {}",
					e.getMessage(), e.getCause(),
					Arrays.toString(e.getStackTrace()));
			throw new RuntimeException("Valintatietojen haku epäonnistui!", e);
		}

	}

	private ValinnanvaiheTyyppi createValinnanvaiheTyyppi(
			ValinnanvaiheDTO valinnanvaihe) {
		ValinnanvaiheTyyppi v = new ValinnanvaiheTyyppi();
		v.setValinnanvaihe(valinnanvaihe.getJarjestysnumero());
		v.setValinnanvaiheOid(valinnanvaihe.getValinnanvaiheoid());
		for (ValintatapajonoDTO vt : valinnanvaihe.getValintatapajono()) {
			v.getValintatapajono().add(createValintatapajonoTyyppi(vt));
		}
		return v;
	}

	private ValintatapajonoTyyppi createValintatapajonoTyyppi(
			ValintatapajonoDTO vt) {
		ValintatapajonoTyyppi valintatapajonoTyyppi = new ValintatapajonoTyyppi();
		valintatapajonoTyyppi.setOid(vt.getOid());
		valintatapajonoTyyppi.setAloituspaikat(vt.getAloituspaikat());
		valintatapajonoTyyppi.setNimi(vt.getNimi());
		valintatapajonoTyyppi.setPrioriteetti(vt.getPrioriteetti());
		valintatapajonoTyyppi.setSiirretaanSijoitteluun(vt
				.isSiirretaanSijoitteluun());
		valintatapajonoTyyppi.setEiVarasijatayttoa(vt.getEiVarasijatayttoa());
		if (vt.getTasasijasaanto() != null) {
			valintatapajonoTyyppi.setTasasijasaanto(TasasijasaantoTyyppi
					.valueOf(vt.getTasasijasaanto().name()));
		}

		for (JonosijaDTO jonosija : vt.getJonosijat()) {
			HakijaTyyppi ht = new HakijaTyyppi();
			ht.setPrioriteetti(jonosija.getPrioriteetti());

			if (jonosija.getTuloksenTila() == null) {
				ht.setTila(HakemusTilaTyyppi.MAARITTELEMATON);
			} else {
				ht.setTila(HakemusTilaTyyppi.valueOf(jonosija.getTuloksenTila()
						.name()));
			}

			ht.setHakemusOid(jonosija.getHakemusOid());
			ht.setEtunimi(jonosija.getEtunimi());
			ht.setSukunimi(jonosija.getSukunimi());
			ht.setOid(jonosija.getHakijaOid());
			ht.setJonosija(jonosija.getJonosija());
			for (SyotettyArvoDTO sa : jonosija.getSyotetytArvot()) {
				ht.getSyotettyArvo().add(createSyotettyArvoTyyppi(sa));

			}

			if (jonosija.isHarkinnanvarainen()) {
				ht.setHarkinnanvarainen(Boolean.TRUE);
			}

			if (!jonosija.getJarjestyskriteerit().isEmpty()) {
				JarjestyskriteeritulosDTO merkityksellisinKriteeri = jonosija
						.getJarjestyskriteerit().first();
				try {

					if (merkityksellisinKriteeri.getKuvaus() != null
							|| !merkityksellisinKriteeri.getKuvaus().isEmpty()) {
						ht.getTilanKuvaus().addAll(
								convertKuvaus(merkityksellisinKriteeri
										.getKuvaus()));
					}
				} catch (Exception e) {
					LOG.error(
							"Järjestyskriteerille ei voitu luoda kuvausta: {}",
							e.getMessage());
					throw new RuntimeException(e);
				}

				BigDecimal arvo = merkityksellisinKriteeri.getArvo();
				if (arvo == null) {
					ht.setPisteet(StringUtils.EMPTY);
				} else {
					ht.setPisteet(arvo.toString());
				}

			}

			valintatapajonoTyyppi.getHakija().add(ht);
		}
		return valintatapajonoTyyppi;
	}

	private Collection<AvainArvoTyyppi> convertKuvaus(Map<String, String> kuvaus) {
		Collection<AvainArvoTyyppi> a = Lists.newArrayList();
		for (Entry<String, String> keyValuePair : kuvaus.entrySet()) {
			AvainArvoTyyppi a0 = new AvainArvoTyyppi();
			a0.setAvain(keyValuePair.getKey());
			a0.setArvo(keyValuePair.getValue());
			a.add(a0);
		}
		return a;
	}

	private SyotettyArvoTyyppi createSyotettyArvoTyyppi(SyotettyArvoDTO sa) {
		SyotettyArvoTyyppi tyyppi = new SyotettyArvoTyyppi();
		tyyppi.setArvo(sa.getArvo());
		tyyppi.setLaskennallinenArvo(sa.getLaskennallinenArvo());
		tyyppi.setOsallistuminen(sa.getOsallistuminen());
		tyyppi.setTunniste(sa.getTunniste());
		return tyyppi;
	}

}
