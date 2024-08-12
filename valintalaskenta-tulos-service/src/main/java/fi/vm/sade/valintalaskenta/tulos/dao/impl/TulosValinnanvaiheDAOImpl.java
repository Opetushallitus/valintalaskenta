package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu.SijoitteluJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.sijoittelu.SijoitteluValintatapajono;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosValinnanvaiheRepository;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class TulosValinnanvaiheDAOImpl implements TulosValinnanvaiheDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(TulosValinnanvaiheDAOImpl.class);

  private final TulosValinnanvaiheRepository repo;

  public TulosValinnanvaiheDAOImpl(TulosValinnanvaiheRepository repo) {
    this.repo = repo;
  }

  @Override
  public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
    return repo.findDistinctValinnanvaihesByHakukohdeOid(hakukohdeoid);
  }

  @Override
  public List<Valinnanvaihe> readByHakuOid(String hakuoid) {
    return repo.findDistinctValinnanvaihesByHakuOidAsList(hakuoid);
  }

  @Override
  public Stream<Valinnanvaihe> readByHakuOidStreaming(String hakuoid) {
    return repo.findDistinctValinnanvaihesByHakuOid(hakuoid);
  }

  @Override
  public List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    return repo.findDistinctValinnanvaihesByHakuOidAndHakemusOid(hakuOid, hakemusOid);
  }

  @Override
  public Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid) {
    return repo.findValinnanvaiheByValintatapajono(valintatapajonoOid).orElse(null);
  }

  @Override
  public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
    return repo.findValinnanvaiheByValinnanvaiheOid(valinnanvaiheOid).orElse(null);
  }

  @Override
  public void saveOrUpdate(Valinnanvaihe vaihe) {
    if (vaihe.getCreatedAt() == null) {
      vaihe.setCreatedAt(new Date());
    }
    repo.save(vaihe);
  }

  @Override
  public UUID saveVaihe(Valinnanvaihe vaihe) {
    if (vaihe.getCreatedAt() == null) {
      vaihe.setCreatedAt(new Date());
    }
    return repo.save(vaihe).getId();
  }

  @Override
  public List<SijoitteluValintatapajono> haeValintatapajonotValinnanvaiheetSijoittelulle(
      String hakuOid) {
    return repo.findValintatapajonoWithValinnanvaiheForSijoittelu(hakuOid);
  }

  @Override
  public List<SijoitteluJonosija> haeJarjestyskriteerituloksetJonosijoillaHaulle(String hakuOid) {
    return repo.haeSijoittelunJonosijatJaJarjestyskriteerit(hakuOid);
  }

  @Override
  public List<String> readNewOrModifiedHakukohdeOids(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    return repo.findHakukohdeOidsByTimeRange(startDatetime, endDatatime);
  }
}
