package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valinta.dokumenttipalvelu.dto.ObjectEntity;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosJarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import fi.vm.sade.valintalaskenta.tulos.service.JarjestyskriteerihistoriaService;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletionException;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.s3.model.NoSuchKeyException;

import javax.annotation.processing.Completion;

@Service
public class JarjestyskriteerihistoriaServiceImpl implements JarjestyskriteerihistoriaService {

  private static final Logger LOG =
      LoggerFactory.getLogger(JarjestyskriteerihistoriaServiceImpl.class);

  private final Dokumenttipalvelu dokumenttipalvelu;

  private final TulosJarjestyskriteerihistoriaDAO historiaDAO;

  public JarjestyskriteerihistoriaServiceImpl(
      TulosJarjestyskriteerihistoriaDAO historiaDAO, Dokumenttipalvelu dokumenttipalvelu) {
    this.historiaDAO = historiaDAO;
    this.dokumenttipalvelu = dokumenttipalvelu;
  }

  @Override
  public List<Jarjestyskriteerihistoria> findByValintatapajonoAndHakemusOid(
      String valintatapajonoOid, String hakemusOid) {
    List<UUID> historyIds =
        historiaDAO.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid);
    List<Jarjestyskriteerihistoria> historiat = getJarjestyskriteerihistoriatFromS3(historyIds);
    List<UUID> puuttuvatHistoriat = CollectionUtils.subtract(historyIds,
        historiat.stream().map(Jarjestyskriteerihistoria::getId).toList()).stream().toList();
    return puuttuvatHistoriat.isEmpty() ? historiat : ListUtils.union(historiat, getJarjestyskriteerihistoriatFromDb(puuttuvatHistoriat));
  }

  private List<Jarjestyskriteerihistoria> getJarjestyskriteerihistoriatFromDb(List<UUID> ids) {
    List<Jarjestyskriteerihistoria> historiat = historiaDAO.findById(ids);
    if (historiat.size() < ids.size()) {
      LOG.error("Jarjestyskriteerihistorioita ei löytynyt dokumenttipalvelusta tai kannasta, puuttuvat historiat: {}",
        CollectionUtils.subtract(ids, historiat.stream().map(Jarjestyskriteerihistoria::getId).toList()).stream());
      throw new RuntimeException("Jarjestyskriteerihistorioita ei löytynyt kannasta");
    }
    return historiat;
  }

  private List<Jarjestyskriteerihistoria> getJarjestyskriteerihistoriatFromS3(
      List<UUID> historyIds) {
    return historyIds.stream()
        .map(this::getJarjestyskriteerihistoria)
        .filter(Objects::nonNull)
        .toList();
  }

  private Jarjestyskriteerihistoria getJarjestyskriteerihistoria(UUID id) {
    Jarjestyskriteerihistoria historia = new Jarjestyskriteerihistoria();
    historia.setId(id);
    try {
      String key = dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, id.toString());
      ObjectEntity objectEntity = dokumenttipalvelu.get(key);
      historia.setHistoriaGzip(objectEntity.entity.readAllBytes());
    } catch (IOException e) {
      LOG.error("Tietojen lataus epäonnistui järjestyskriteerihistorialle {}", id);
      throw new RuntimeException(e);
    } catch (NoSuchKeyException | CompletionException e) {
      LOG.debug(
          "Järjestyskriteerihistoriaa {} ei löytynyt s3:sta, yritetään hakea se tieto kannasta",
          id);
      return null;
    }
    return JarjestyskriteeriKooderi.dekoodaa(historia);
  }
}
