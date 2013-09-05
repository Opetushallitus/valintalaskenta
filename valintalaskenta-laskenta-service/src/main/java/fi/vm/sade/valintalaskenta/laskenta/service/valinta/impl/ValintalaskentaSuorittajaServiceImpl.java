package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.kaava.Laskentadomainkonvertteri;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.Lukuarvofunktio;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.model.Funktiotyyppi;
import fi.vm.sade.service.valintaperusteet.schema.JarjestyskriteeriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.TavallinenValinnanVaiheTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteetTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.ValintatapajonoJarjestyskriteereillaTyyppi;
import fi.vm.sade.service.valintaperusteet.service.validointi.virhe.LaskentakaavaEiOleValidiException;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.FunktioKutsuTyyppiToFunktioKutsuConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion.HakemusTyyppiToHakemusConverter;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.Hakemuslaskin;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.ValintalaskentaSuorittajaService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * @author Jussi Jartamo
 */
@Service
public class ValintalaskentaSuorittajaServiceImpl implements ValintalaskentaSuorittajaService {

    private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaSuorittajaServiceImpl.class);

    @Autowired
    private HakemusTyyppiToHakemusConverter hakemusConverter;

    @Autowired
    private FunktioKutsuTyyppiToFunktioKutsuConverter funktiokutsuConverter;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private JarjestyskriteerihistoriaDAO jarjestyskriteerihistoriaDAO;

    @Autowired
    private Hakemuslaskin hakemuslaskin;

    @Override
    public void suoritaLaskenta(List<HakemusTyyppi> kaikkiHakemukset, List<ValintaperusteetTyyppi> valintaperusteet) {

        Map<String, Hakemukset> hakemuksetHakukohteittain = jarjestaHakemuksetHakukohteittain(kaikkiHakemukset);

        // Järjestetään valintaperusteet valinnan vaiheiden järjestysnumeron mukaan
        Collections.sort(valintaperusteet, new Comparator<ValintaperusteetTyyppi>() {
            @Override
            public int compare(ValintaperusteetTyyppi o1, ValintaperusteetTyyppi o2) {
                return o1.getValinnanVaihe().getValinnanVaiheJarjestysluku() - o2.getValinnanVaihe().getValinnanVaiheJarjestysluku();
            }
        });

        for (ValintaperusteetTyyppi vp : valintaperusteet) {
            String hakuOid = vp.getHakuOid();
            String hakukohdeOid = vp.getHakukohdeOid();
            String hakuoid = vp.getHakuOid();
            String tarjoajaOid = vp.getTarjoajaOid();

            List<HakemusWrapper> hakemukset = hakemuksetHakukohteittain.get(hakukohdeOid).getHakemukset();

            List<Hakemus> laskentahakemukset = hakemuksetHakukohteittain.get(hakukohdeOid).getLaskentahakemukset();
            if (hakemukset == null || hakemukset.isEmpty() ||
                    !(vp.getValinnanVaihe() instanceof TavallinenValinnanVaiheTyyppi)) {
                continue;
            }

            TavallinenValinnanVaiheTyyppi vaihe = (TavallinenValinnanVaiheTyyppi) vp.getValinnanVaihe();

            final String valinnanvaiheOid = vaihe.getValinnanVaiheOid();
            final int jarjestysnumero = vaihe.getValinnanVaiheJarjestysluku();

            Valinnanvaihe edellinenVaihe = valinnanvaiheDAO.haeEdellinenValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero);
            Valinnanvaihe valinnanvaihe = haeTaiLuoValinnanvaihe(valinnanvaiheOid);
            valinnanvaihe.setHakukohdeOid(hakukohdeOid);
            valinnanvaihe.setHakuOid(hakuoid);
            valinnanvaihe.setJarjestysnumero(jarjestysnumero);
            valinnanvaihe.setValinnanvaiheOid(valinnanvaiheOid);
            valinnanvaihe.setTarjoajaOid(tarjoajaOid);

            for (ValintatapajonoJarjestyskriteereillaTyyppi j : vaihe.getValintatapajono()) {
                Valintatapajono jono = new Valintatapajono();
                jono.setAloituspaikat(j.getAloituspaikat());
                jono.setEiVarasijatayttoa(j.isEiVarasijatayttoa());
                jono.setNimi(j.getNimi());
                jono.setPrioriteetti(j.getPrioriteetti());
                jono.setSiirretaanSijoitteluun(j.isSiirretaanSijoitteluun());
                jono.setTasasijasaanto(Tasasijasaanto.valueOf(j.getTasasijasaanto().name()));
                jono.setValintatapajonoOid(j.getOid());

                Map<String, Jonosija> jonosijatHakemusOidinMukaan = new HashMap<String, Jonosija>();
                for (JarjestyskriteeriTyyppi jk : j.getJarjestyskriteerit()) {
                    try {
                        Jarjestyskriteeritulos jktulos = new Jarjestyskriteeritulos();
                        jktulos.setPrioriteetti(jk.getPrioriteetti());

                        Funktiokutsu funktiokutsu = funktiokutsuConverter.convert(jk.getFunktiokutsu());
                        if (!Funktiotyyppi.LUKUARVOFUNKTIO.equals(funktiokutsu.getFunktionimi().getTyyppi())) {
                            LOG.error("Valintatapajonon {} prioriteetilla {} olevan järjestyskriteerin laskentakaava " +
                                    "ei ole tyypiltään " + Funktiotyyppi.LUKUARVOFUNKTIO.name() + ". Laskentaa ei " +
                                    "voida suorittaa.", new Object[]{j.getOid(), jk.getPrioriteetti()});
                            continue;
                        }

                        Lukuarvofunktio lukuarvofunktio = Laskentadomainkonvertteri.muodostaLukuarvolasku(funktiokutsu);
                        for (HakemusWrapper hw : hakemukset) {
                            hakemuslaskin.suoritaLaskentaHakemukselle(hakukohdeOid, hw, laskentahakemukset,
                                    lukuarvofunktio, jktulos, edellinenVaihe, jonosijatHakemusOidinMukaan);
                        }
                    } catch (LaskentakaavaEiOleValidiException e) {
                        LOG.error("Valintatapajonon {} prioriteetilla {} olevan järjestyskriteerin " +
                                "laskentakaava ei ole validi. Laskentaa ei voida suorittaa.",
                                new Object[]{j.getOid(), jk.getPrioriteetti()});
                        continue;
                    }
                }

                jono.getJonosijat().addAll(jonosijatHakemusOidinMukaan.values());
                valinnanvaihe.getValintatapajonot().add(jono);
            }

            valinnanvaiheDAO.create(valinnanvaihe);
        }
    }

    private Map<String, Hakemukset> jarjestaHakemuksetHakukohteittain(List<HakemusTyyppi> hakemukset) {
        Map<String, Hakemukset> hakukohdeHakemukset = new HashMap<String, Hakemukset>();
        for (HakemusTyyppi hakemus : hakemukset) {
            for (HakukohdeTyyppi hakukohde : hakemus.getHakutoive()) {
                String hakukohdeoid = hakukohde.getHakukohdeOid();
                if (!hakukohdeHakemukset.containsKey(hakukohdeoid)) {
                    hakukohdeHakemukset.put(hakukohdeoid, new Hakemukset());
                }
                HakemusWrapper h = new HakemusWrapper();
                h.setHakemusTyyppi(hakemus);
                h.setLaskentahakemus(hakemusConverter.convert(hakemus));

                for (HakukohdeTyyppi hakutoive : hakemus.getHakutoive()) {
                    if (hakukohdeoid.equals(hakutoive.getHakukohdeOid())) {
                        h.setHakutoiveprioriteetti(hakutoive.getPrioriteetti());
                        break;
                    }
                }

                hakukohdeHakemukset.get(hakukohdeoid).getHakemukset().add(h);
                hakukohdeHakemukset.get(hakukohdeoid).getLaskentahakemukset().add(h.getLaskentahakemus());
            }
        }
        return hakukohdeHakemukset;
    }

    private Valinnanvaihe haeTaiLuoValinnanvaihe(String valinnanvaiheOid) {
        Valinnanvaihe valinnanvaihe = valinnanvaiheDAO.haeValinnanvaihe(valinnanvaiheOid);

        // Poistetaan vanhat historiat
        if (valinnanvaihe != null) {
            for (Valintatapajono jono : valinnanvaihe.getValintatapajonot()) {
                for (Jonosija jonosija : jono.getJonosijat()) {
                    for (Jarjestyskriteeritulos tulos : jonosija.getJarjestyskriteeritulokset()) {
                        jarjestyskriteerihistoriaDAO.delete(tulos.getHistoria());
                    }
                }
            }

            valinnanvaihe.getValintatapajonot().clear();
        } else {
            valinnanvaihe = new Valinnanvaihe();
        }

        return valinnanvaihe;
    }
}
