package fi.vm.sade.valintalaskenta.tulos.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import fi.vm.sade.valintalaskenta.dao.HakukohdeDAO;
import fi.vm.sade.valintalaskenta.dao.JarjestyskriteeritulosDAO;
import fi.vm.sade.valintalaskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.dao.ValintatapajonoDAO;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Service
public class ValintalaskentaTulosServiceImpl implements ValintalaskentaTulosService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValintalaskentaTulosServiceImpl.class);

    @Autowired
    private HakukohdeDAO hakukohdeDAO;

    @Autowired
    private JarjestyskriteeritulosDAO jarjestyskriteeritulosDAO;

    @Autowired
    private ValinnanvaiheDAO valinnanvaiheDAO;

    @Autowired
    private ValintatapajonoDAO valintatapajonoDAO;

    public List<Versioituhakukohde> haeHakukohteet() {
        return hakukohdeDAO.readAll();
    }

    public List<Versioituhakukohde> haeHakukohteetHaulle(String hakuoid) {
        return hakukohdeDAO.readByHakuOid(hakuoid);

    }

    public List<Jarjestyskriteeritulos> haeJarjestyskriteerituloksetValintatapajonolle(String valintatapajonooid) {
        return jarjestyskriteeritulosDAO.readByValintatapajonoOid(valintatapajonooid);
    }

    public List<Valinnanvaihe> haeValinnanvaiheetHakukohteelle(String hakukohdeoid) {
        return valinnanvaiheDAO.readByHakukohdeOid(hakukohdeoid);

    }

    public List<Valintatapajono> haeValintatapajonoValinnanvaiheelle(String valinnanvaiheoid) {
        return valintatapajonoDAO.readByValinnanvaiheOid(valinnanvaiheoid);
    }

}