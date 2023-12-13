package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valinta.dokumenttipalvelu.dto.ObjectEntity;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosJarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import fi.vm.sade.valintalaskenta.tulos.service.JarjestyskriteerihistoriaService;
import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletionException;

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
    List<UUID> loytyneetHistoriat =
        historiat.stream().map(Jarjestyskriteerihistoria::getId).toList();
    historiat.addAll(
        historiaDAO.findById(
            historyIds.stream().filter(id -> !loytyneetHistoriat.contains(id)).toList()));
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
      ObjectEntity objectEntity = dokumenttipalvelu.get(dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, id.toString()));
      historia.setHistoriaGzip(objectEntity.entity.readAllBytes());
    } catch (IOException e) {
      LOG.warn("Tietojen lataus epäonnistui järjestyskriteerihistorialle {}", id);
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
