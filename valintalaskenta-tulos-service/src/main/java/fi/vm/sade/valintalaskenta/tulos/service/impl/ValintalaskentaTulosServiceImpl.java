package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.util.*;

import javax.annotation.Nullable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import scala.actors.threadpool.Arrays;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;

import fi.vm.sade.security.service.authz.util.AuthorizationUtil;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;
import fi.vm.sade.valintalaskenta.domain.LogEntry;
import fi.vm.sade.valintalaskenta.domain.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteeritulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.dao.HakemusTulosDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.HakukohdeDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.JonosijaHistoriaTulosDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.MuokattuJonosijaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintatapajonoDAO;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;

/**
 * @author Jussi Jartamo
 */
@Service
public class ValintalaskentaTulosServiceImpl implements ValintalaskentaTulosService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValintalaskentaTulosServiceImpl.class);

    @Autowired
    private HakukohdeDAO hakukohdeDAO;

    // @Autowired
    // private JarjestyskriteeritulosDAO jarjestyskriteeritulosDAO;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private ValintatapajonoDAO valintatapajonoDAO;

    @Autowired
    private ValintakoeOsallistuminenDAO valintakoeOsallistuminenDAO;

    @Autowired
    private MuokattuJonosijaDAO muokattuJonosijaDAO;

    @Autowired
    private ValintatulosConverter valintatulosConverter;

    @Autowired
    private JonosijaHistoriaTulosDAO jonosijaHistoriaTulosDAO;

    @Autowired
    private HakemusTulosDAO hakemusTulosDAO;

    public HakemusDTO haeTuloksetHakemukselle(final String hakuOid, final String hakemusOid) {
        final Set<String> oidit = hakemusTulosDAO.findValintatapajonoOidsByHakemusOid(hakemusOid);
        if (oidit == null || oidit.isEmpty()) {
            LOGGER.debug("Ei oideja! Haku {} ja hakemus {}", new Object[] { hakuOid, hakemusOid });
            return new HakemusDTO(hakuOid, hakemusOid, Collections.<HakukohdeDTO> emptyList());
        } else {
            LOGGER.debug("Oideja loytyi {}", Arrays.toString(oidit.toArray()));
        }
        List<VersiohallintaHakukohde> hakukohteet = hakemusTulosDAO.findByValinnanvaiheOid(hakuOid);
        if (hakukohteet == null || hakukohteet.isEmpty()) {
            LOGGER.debug("Ei hakukohteita! Haku {} ja hakemus {} ja oidit {}", new Object[] { hakuOid, hakemusOid,
                    Arrays.toString(oidit.toArray()) });
            return new HakemusDTO(hakuOid, hakemusOid, Collections.<HakukohdeDTO> emptyList());
        }

        List<HakukohdeDTO> dtot = new ArrayList<HakukohdeDTO>();
        for (VersiohallintaHakukohde haku : hakukohteet) {

            Hakukohde hakukohde = haku.getHakukohteet().haeUusinVersio().getHakukohde();
            Valinnanvaihe valinnanvaihe = hakukohde.getValinnanvaihe();

            valinnanvaihe.getValintatapajono();
            Collection<Valintatapajono> valintatapajonot = Collections2.filter(valinnanvaihe.getValintatapajono(),
                    new Predicate<Valintatapajono>() {
                        public boolean apply(@Nullable Valintatapajono j) {
                            return oidit.contains(j.getOid());
                        }

                    });
            if (valintatapajonot.isEmpty()) {
                continue;
            }
            List<ValintatapajonoDTO> jonot = new ArrayList<ValintatapajonoDTO>();
            for (Valintatapajono jono : valintatapajonot) {

                ValintatapajonoDTO j = new ValintatapajonoDTO();
                j.setAloituspaikat(jono.getAloituspaikat());
                j.setEiVarasijatayttoa(jono.getEiVarasijatayttoa());
                j.setNimi(jono.getNimi());
                j.setOid(jono.getOid());
                j.setPrioriteetti(jono.getPrioriteetti());
                j.setSiirretaanSijoitteluun(jono.isSiirretaanSijoitteluun());
                j.setTasasijasaanto(jono.getTasasijasaanto());
                j.setVersio(jono.getVersio());
                // Palautetaan ainoastaan hakemukseen liittyvat jonosijat!
                Collection<Jonosija> jonosijat = Collections2.filter(jono.getJonosijat(), new Predicate<Jonosija>() {
                    public boolean apply(@Nullable Jonosija js) {
                        return hakemusOid.equals(js.getHakemusoid());
                    }
                });
                j.setJonosijat(valintatulosConverter.convertJonosija(jonosijat));
                jonot.add(j);
            }
            ValinnanvaiheDTO v = new ValinnanvaiheDTO();
            v.setCreatedAt(valinnanvaihe.getCreatedAt());
            v.setJarjestysnumero(valinnanvaihe.getJarjestysnumero());
            v.setValinnanvaiheoid(valinnanvaihe.getValinnanvaiheoid());
            List<ValinnanvaiheDTO> valinnanvaiheet = new ArrayList<ValinnanvaiheDTO>();
            valinnanvaiheet.add(v);
            HakukohdeDTO h = new HakukohdeDTO();
            h.setCreatedAt(hakukohde.getCreatedAt());
            h.setHakuoid(hakukohde.getHakuoid());
            h.setTarjoajaoid(hakukohde.getTarjoajaoid());
            h.setOid(hakukohde.getOid());
            h.setValinnanvaihe(valinnanvaiheet);
            v.setValintatapajono(jonot);
            dtot.add(h);
        }
        return new HakemusDTO(hakuOid, hakemusOid, dtot);
    }

    private void applyMuokatutJonosijatToValinannvaihe(String hakukohdeoid, List<ValinnanvaiheDTO> b) {
        List<MuokattuJonosija> a = muokattuJonosijaDAO.readByhakukohdeOid(hakukohdeoid);
        applyMuokatutJonosijat(b, a);
    }

    private void applyMuokatutJonosijatToHakukohde(String hakuOid, List<HakukohdeDTO> b) {
        List<MuokattuJonosija> a = muokattuJonosijaDAO.readByHakuOid(hakuOid);
        for (HakukohdeDTO hakukohde : b) {
            applyMuokatutJonosijat(hakukohde.getValinnanvaihe(), a);
        }
    }

    private void applyMuokatutJonosijat(List<ValinnanvaiheDTO> b, List<MuokattuJonosija> a) {
        for (ValinnanvaiheDTO dto : b) {
            for (ValintatapajonoDTO valintatapajonoDTO : dto.getValintatapajono()) {
                for (JonosijaDTO jonosija : valintatapajonoDTO.getJonosijat()) {
                    for (MuokattuJonosija muokattuJonosija : a) {
                        if (muokattuJonosija.getHakemusOid().equals(jonosija.getHakemusOid())
                                && valintatapajonoDTO.getOid().equals(muokattuJonosija.getValintatapajonoOid())) {
                            applyJonosija(jonosija, muokattuJonosija);
                        }
                    }
                }
                valintatulosConverter.sort(valintatapajonoDTO.getJonosijat());
            }
        }
    }

    private void applyJonosija(JonosijaDTO jonosijaDTO, MuokattuJonosija muokattuJonosija) {
        boolean jonosijaMuokattu = false;

        if (muokattuJonosija.getHarkinnanvarainen() != null) {
            jonosijaDTO.setHarkinnanvarainen(muokattuJonosija.getHarkinnanvarainen());
            jonosijaMuokattu = true;
        }
        if (muokattuJonosija.getPrioriteetti() != null) {
            jonosijaDTO.setPrioriteetti(muokattuJonosija.getPrioriteetti());
            jonosijaMuokattu = true;
        }

        for (Integer i : muokattuJonosija.getJarjestyskriteerit().keySet()) {
            Jarjestyskriteeritulos muokattuJarjestyskriteeritulos = muokattuJonosija.getJarjestyskriteerit().get(i);

            JarjestyskriteeritulosDTO alkuperainenJarjestyskriteeritulosDTO = jonosijaDTO.getJarjestyskriteerit()
                    .get(i);
            if (alkuperainenJarjestyskriteeritulosDTO == null) {
                alkuperainenJarjestyskriteeritulosDTO = new JarjestyskriteeritulosDTO();
                jonosijaDTO.getJarjestyskriteerit().put(i, alkuperainenJarjestyskriteeritulosDTO);
            }
            if (muokattuJarjestyskriteeritulos.getArvo() != null) {
                alkuperainenJarjestyskriteeritulosDTO.setArvo(muokattuJarjestyskriteeritulos.getArvo());
                jonosijaMuokattu = true;
            }
            if (muokattuJarjestyskriteeritulos.getKuvaus() != null) {
                alkuperainenJarjestyskriteeritulosDTO.setKuvaus(muokattuJarjestyskriteeritulos.getKuvaus());
                jonosijaMuokattu = true;
            }
            if (muokattuJarjestyskriteeritulos.getTila() != null) {
                alkuperainenJarjestyskriteeritulosDTO.setTila(muokattuJarjestyskriteeritulos.getTila());
                jonosijaMuokattu = true;
            }
        }
        if (jonosijaMuokattu) {
            jonosijaDTO.setMuokattu(true);
        }
    }

    @Override
    public List<ValinnanvaiheDTO> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {
        List<Valinnanvaihe> a = valinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);
        List<ValinnanvaiheDTO> b = valintatulosConverter.convertValinnanvaiheList(a);
        applyMuokatutJonosijatToValinannvaihe(hakukohdeoid, b);
        return b;

    }

    @Override
    public List<HakukohdeDTO> haeVirheetHaulle(String hakuOid) {

        // FIXME: Suora mongo kysely tälle.
        List<Hakukohde> a = valinnanvaiheDAO.readByHakuOid(hakuOid);
        List<HakukohdeDTO> b = valintatulosConverter.convertHakukohde(a);
        Iterator<HakukohdeDTO> hakukohdeIter = b.iterator();
        while (hakukohdeIter.hasNext()) {
            HakukohdeDTO hakukohdeDTO = hakukohdeIter.next();

            Iterator<ValinnanvaiheDTO> vvIter = hakukohdeDTO.getValinnanvaihe().iterator();

            while (vvIter.hasNext()) {

                ValinnanvaiheDTO vv = vvIter.next();

                Iterator<ValintatapajonoDTO> vtjIter = vv.getValintatapajono().iterator();

                while (vtjIter.hasNext()) {
                    ValintatapajonoDTO valintatapajonoDTO = vtjIter.next();

                    Iterator<JonosijaDTO> jonoIter = valintatapajonoDTO.getJonosijat().iterator();

                    while (jonoIter.hasNext()) {
                        JonosijaDTO jonosijaDTO = jonoIter.next();

                        Iterator<Map.Entry<Integer, JarjestyskriteeritulosDTO>> iterator = jonosijaDTO.getJarjestyskriteerit().entrySet().iterator();
                        while (iterator.hasNext()) {
                            Map.Entry<Integer, JarjestyskriteeritulosDTO> next = iterator.next();
                            if (!next.getValue().getTila().equals(JarjestyskriteerituloksenTila.VIRHE)) {
                                iterator.remove();
                            }
                        }

                        if (jonosijaDTO.getJarjestyskriteerit().size() == 0) {
                            jonoIter.remove();
                        }
                    }

                    if (valintatapajonoDTO.getJonosijat().size() == 0) {
                        vtjIter.remove();
                    }
                }

                if (vv.getValintatapajono().size() == 0) {
                    vvIter.remove();
                }
            }

            if (hakukohdeDTO.getValinnanvaihe().size() == 0) {
                hakukohdeIter.remove();
            }

        }
        return b;
    }

    @Override
    public List<HakukohdeDTO> haeLasketutValinnanvaiheetHaulle(String hakuOid) {
        List<Hakukohde> a = valinnanvaiheDAO.readByHakuOid(hakuOid);
        List<HakukohdeDTO> b = valintatulosConverter.convertHakukohde(a);
        applyMuokatutJonosijatToHakukohde(hakuOid, b);
        return b;
    }

    @Override
    public List<ValintakoeOsallistuminen> haeValintakoeOsallistumiset(String hakijaoid) {
        return valintakoeOsallistuminenDAO.findByHakijaOid(hakijaoid);
    }

    @Override
    public List<ValintakoeOsallistuminen> haeValintakoeOsallistumisetByHakutoive(String hakukohdeOid) {
        return valintakoeOsallistuminenDAO.findByHakutoive(hakukohdeOid);
    }

    @Override
    public List<JonosijaHistoria> haeJonosijaHistoria(String valintatapajonoOid, String hakemusOid) {
        return jonosijaHistoriaTulosDAO.findByValintatapajonoAndVersioAndHakemusOid(valintatapajonoOid, hakemusOid);
    }

    @Override
    public MuokattuJonosija muutaJarjestyskriteeri(String valintatapajonoOid, String hakemusOid,
            Integer jarjestyskriteeriPrioriteetti, MuokattuJonosijaDTO jonosija, String selite) {

        Valintatapajono valintatapajono = valintatapajonoDAO.findByOid(valintatapajonoOid);
        VersiohallintaHakukohde hakukohde = hakukohdeDAO.findByValintatapajono(valintatapajono);

        MuokattuJonosija muokattuJonosija;
        muokattuJonosija = muokattuJonosijaDAO.readByValintatapajonoOid(valintatapajonoOid, hakemusOid);
        if (muokattuJonosija == null) {
            muokattuJonosija = new MuokattuJonosija();
        }

        muokattuJonosija.setHakemusOid(hakemusOid);
        muokattuJonosija.setValintatapajonoOid(valintatapajonoOid);
        muokattuJonosija.setHakuOid(hakukohde.getHakuoid());
        muokattuJonosija.setHakukohdeOid(hakukohde.getHakukohdeoid());

        Jarjestyskriteeritulos jarjestyskriteeritulos = muokattuJonosija.getJarjestyskriteerit().get(
                jarjestyskriteeriPrioriteetti);
        if (jarjestyskriteeritulos == null) {
            jarjestyskriteeritulos = new Jarjestyskriteeritulos();
            muokattuJonosija.getJarjestyskriteerit().put(jarjestyskriteeriPrioriteetti, jarjestyskriteeritulos);
        }
        jarjestyskriteeritulos.setKuvaus("Muokattu käsin");
        jarjestyskriteeritulos.setArvo(jonosija.getArvo());
        jarjestyskriteeritulos.setTila(jonosija.getTila());

        addLogEntry(selite, muokattuJonosija, "jarjestyskriteeriPrioriteetti: " + jarjestyskriteeriPrioriteetti
                + " arvo: " + jonosija.getArvo() + " tila: " + jonosija.getTila().name());

        muokattuJonosijaDAO.saveOrUpdate(muokattuJonosija);

        return muokattuJonosija;
    }

    private void addLogEntry(String selite, MuokattuJonosija muokattuJonosija, String muutos) {
        LogEntry logEntry = new LogEntry();

        logEntry.setLuotu(new Date());
        logEntry.setMuokkaaja(AuthorizationUtil.getCurrentUser());
        logEntry.setSelite(selite);
        logEntry.setMuutos(muutos);

        muokattuJonosija.getLogEntries().add(logEntry);
    }

}
