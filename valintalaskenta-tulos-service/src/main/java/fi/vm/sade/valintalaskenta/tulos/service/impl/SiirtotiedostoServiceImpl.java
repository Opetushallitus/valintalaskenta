package fi.vm.sade.valintalaskenta.tulos.service.impl;

import static fi.vm.sade.valintalaskenta.tulos.dao.PoistettuDAO.EntityType.*;

import com.google.common.collect.Lists;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintakoeOsallistuminenDTO.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintakoeOsallistuminenDTO.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintakoeOsallistuminenDTO.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintakoeOsallistuminenDTO.ValintakoeValinnanVaihe;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintalaskennanTulosDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintalaskennanTulosDTO.Jonosija;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintalaskennanTulosDTO.ValinnanVaihe;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PoistettuValintalaskennanTulosDTO.ValintatapaJono;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.ValintakoeOsallistuminenSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.ValintatietoValinnanvaiheSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.siirtotiedosto.Poistettu;
import fi.vm.sade.valintalaskenta.tulos.SiirtotiedostoS3Client;
import fi.vm.sade.valintalaskenta.tulos.dao.PoistettuDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.SiirtotiedostoService;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

@Service
@ConditionalOnProperty("valintalaskenta.enable-siirtotiedosto-service")
public class SiirtotiedostoServiceImpl implements SiirtotiedostoService {
  private static final Logger LOGGER = LoggerFactory.getLogger(SiirtotiedostoServiceImpl.class);

  private final TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO;
  private final TulosValinnanvaiheDAO tulosValinnanvaiheDAO;
  private final PoistettuDAO poistettuDAO;
  private final ValintalaskentaTulosService valintalaskentaTulosService;
  private final ValintalaskentaModelMapper modelMapper;
  private final ValintatulosConverter valintatulosConverter;

  private final SiirtotiedostoS3Client siirtotiedostoS3Client;

  @Autowired
  public SiirtotiedostoServiceImpl(
      final TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO,
      final TulosValinnanvaiheDAO tulosValinnanvaiheDAO,
      final PoistettuDAO poistettuDAO,
      final ValintalaskentaTulosService tulosService,
      final ValintalaskentaModelMapper modelMapper,
      final ValintatulosConverter valintatulosConverter,
      final SiirtotiedostoS3Client siirtotiedostoS3Client) {
    this.tulosValintakoeOsallistuminenDAO = tulosValintakoeOsallistuminenDAO;
    this.tulosValinnanvaiheDAO = tulosValinnanvaiheDAO;
    this.poistettuDAO = poistettuDAO;
    this.valintalaskentaTulosService = tulosService;
    this.modelMapper = modelMapper;
    this.valintatulosConverter = valintatulosConverter;
    this.siirtotiedostoS3Client = siirtotiedostoS3Client;
  }

  @Override
  public String createSiirtotiedostotForValintakoeOsallistumiset(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    String opId = UUID.randomUUID().toString();
    List<String> hakemusOids =
        tulosValintakoeOsallistuminenDAO.readNewOrModifiedHakemusOids(startDatetime, endDatatime);
    List<List<String>> partitions =
        Lists.partition(hakemusOids, siirtotiedostoS3Client.getMaxHakemusCountInFile());
    List<String> siirtotiedostoKeys = new ArrayList<>();
    for (List<String> hakemusOidChunk : partitions) {
      List<ValintakoeOsallistuminenDTO> osallistumiset = new ArrayList<>();
      for (String hakemusOid : hakemusOidChunk) {
        osallistumiset.add(
            modelMapper.map(
                valintalaskentaTulosService.haeValintakoeOsallistumiset(hakemusOid),
                ValintakoeOsallistuminenDTO.class));
      }
      List<ValintakoeOsallistuminenSiirtotiedostoDTO> osallistumisetAsSiirtotiedostoData =
          valintatulosConverter.convertValintakoeOsallistuminenListForSiirtotiedosto(
              osallistumiset);
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForTulosdata(
              osallistumisetAsSiirtotiedostoData,
              "valintakoe_osallistuminen",
              opId,
              siirtotiedostoKeys.size() + 1));
    }
    PoistettuValintakoeOsallistuminenDTO poistetut =
        findPoistetutValintakoeOsallistumiset(startDatetime, endDatatime);
    if (!poistetut.getPoistetut().isEmpty()) {
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForTulosdata(
              poistetut.getPoistetut(),
              "valintakoe_osallistuminen",
              opId,
              siirtotiedostoKeys.size() + 1));
    }
    LOGGER.info(
        "Kirjoitettiin yhteensä {} hakemuksen valintakoeosallistumiset {} siirtotiedostoon.",
        hakemusOids.size() + poistetut.getPoistetut().size(),
        siirtotiedostoKeys.size());
    return resultJson(siirtotiedostoKeys, hakemusOids.size());
  }

  private PoistettuValintakoeOsallistuminenDTO findPoistetutValintakoeOsallistumiset(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    List<Poistettu> poistetutValintakoeOsallistumiset =
        poistettuDAO.findPoistetut(VALINTAKOE_OSALLISTUMINEN, startDatetime, endDatatime);
    List<Poistettu> poistetutHakutoiveet =
        poistettuDAO.findPoistetut(HAKUTOIVE, startDatetime, endDatatime);
    List<Poistettu> poistetutValintakoeValinnanvaiheet =
        poistettuDAO.findPoistetut(VALINTAKOE_VALINNANVAIHE, startDatetime, endDatatime);
    List<Poistettu> poistetutValintakokeet =
        poistettuDAO.findPoistetut(VALINTAKOE, startDatetime, endDatatime);

    List<UUID> requiredParentValintakoeValinnanvaiheet =
        new ArrayList<>(poistetutValintakokeet.stream().map(Poistettu::getParentId).toList());
    List<Poistettu> valintakoeValinnanvaiheet =
        findAllRequiredParents(
            VALINTAKOE_VALINNANVAIHE,
            poistetutValintakoeValinnanvaiheet,
            requiredParentValintakoeValinnanvaiheet);

    List<UUID> requiredParentHakutoiveet =
        new ArrayList<>(valintakoeValinnanvaiheet.stream().map(Poistettu::getParentId).toList());
    List<Poistettu> hakutoiveet =
        findAllRequiredParents(HAKUTOIVE, poistetutHakutoiveet, requiredParentHakutoiveet);

    List<UUID> requiredParentOsallistumiset =
        new ArrayList<>(hakutoiveet.stream().map(Poistettu::getParentId).toList());
    List<Poistettu> valintakoeOsallistumiset =
        findAllRequiredParents(
            VALINTAKOE_OSALLISTUMINEN,
            poistetutValintakoeOsallistumiset,
            requiredParentOsallistumiset);

    List<ValintakoeOsallistuminen> poistetutOsallistumiset =
        valintakoeOsallistumiset.stream()
            .map(
                osallistuminen -> {
                  ValintakoeOsallistuminen valintakoeOsallistuminen =
                      new ValintakoeOsallistuminen().setHakemusOid(osallistuminen.getTunniste());
                  valintakoeOsallistuminen.setPoistettuItself(osallistuminen.isDeletedItself());
                  if (!osallistuminen.isDeletedItself()) {
                    List<Poistettu> childHakutoiveet = findChildren(hakutoiveet, osallistuminen);
                    List<Hakutoive> poistetutToiveet =
                        childHakutoiveet.stream()
                            .map(
                                toive -> {
                                  Hakutoive hakutoive =
                                      new Hakutoive().setHakukohdeOid(toive.getTunniste());
                                  hakutoive.setPoistettuItself(toive.isDeletedItself());
                                  if (!toive.isDeletedItself()) {
                                    List<Poistettu> childValinnanvaiheet =
                                        findChildren(valintakoeValinnanvaiheet, toive);
                                    List<ValintakoeValinnanVaihe> poistetutVaiheet =
                                        childValinnanvaiheet.stream()
                                            .map(
                                                vaihe -> {
                                                  ValintakoeValinnanVaihe valinnanVaihe =
                                                      new ValintakoeValinnanVaihe()
                                                          .setValinnanVaiheOid(vaihe.getTunniste());
                                                  valinnanVaihe.setPoistettuItself(
                                                      vaihe.isDeletedItself());
                                                  if (!vaihe.isDeletedItself()) {
                                                    List<Poistettu> childKokeet =
                                                        findChildren(poistetutValintakokeet, vaihe);
                                                    List<Valintakoe> valintaKokeet =
                                                        childKokeet.stream()
                                                            .map(
                                                                koe ->
                                                                    new Valintakoe()
                                                                        .setValintakoeOid(
                                                                            koe.getTunniste()))
                                                            .toList();
                                                    valinnanVaihe.setValintakokeet(
                                                        valintaKokeet.isEmpty()
                                                            ? null
                                                            : valintaKokeet);
                                                  }
                                                  return valinnanVaihe;
                                                })
                                            .toList();
                                    hakutoive.setValinnanVaiheet(
                                        poistetutVaiheet.isEmpty() ? null : poistetutVaiheet);
                                  }
                                  return hakutoive;
                                })
                            .toList();
                    valintakoeOsallistuminen.setHakutoiveet(
                        poistetutToiveet.isEmpty() ? null : poistetutToiveet);
                  }
                  return valintakoeOsallistuminen;
                })
            .toList();

    return new PoistettuValintakoeOsallistuminenDTO().setPoistetut(poistetutOsallistumiset);
  }

  private List<Poistettu> findAllRequiredParents(
      PoistettuDAO.EntityType parentType,
      List<Poistettu> poistetutParents,
      List<UUID> requiredParentIds) {
    List<Poistettu> allRequiredParents = new ArrayList<>(poistetutParents);
    Set<UUID> remainingParentIds = new HashSet<>(requiredParentIds);

    idList(allRequiredParents).forEach(remainingParentIds::remove);
    if (!remainingParentIds.isEmpty()) {
      allRequiredParents.addAll(poistettuDAO.findParents(parentType, remainingParentIds));
      idList(allRequiredParents).forEach(remainingParentIds::remove);
    }

    return allRequiredParents;
  }

  private List<UUID> idList(List<Poistettu> items) {
    return items.stream().map(Poistettu::getId).toList();
  }

  private List<Poistettu> findChildren(List<Poistettu> allChildren, Poistettu parent) {
    return allChildren.stream()
        .filter(child -> Objects.equals(child.getParentId(), parent.getId()))
        .toList();
  }

  @Override
  public String createSiirtotiedostotForValintalaskennanTulokset(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    String opId = UUID.randomUUID().toString();
    List<String> hakukohdeOids =
        tulosValinnanvaiheDAO.readNewOrModifiedHakukohdeOids(startDatetime, endDatatime);
    List<List<String>> partitions =
        Lists.partition(hakukohdeOids, siirtotiedostoS3Client.getMaxHakukohdeCountInFile());
    List<String> siirtotiedostoKeys = new ArrayList<>();
    AtomicLong tulosCount = new AtomicLong();
    for (List<String> hakekohdeOidChunk : partitions) {
      List<List<ValintatietoValinnanvaiheSiirtotiedostoDTO>> tulokset = new ArrayList<>();
      for (String hakukohdeOid : hakekohdeOidChunk) {

        tulokset.add(
            valintalaskentaTulosService.haeValinnanvaiheetHakukohteelleForSiirtotiedosto(
                hakukohdeOid));
      }

      tulosCount.addAndGet(tulokset.stream().flatMap(List::stream).count());
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForTulosdata(
              tulokset.stream().flatMap(List::stream).collect(Collectors.toList()),
              "valintalaskennan_tulos",
              opId,
              siirtotiedostoKeys.size() + 1));
    }
    PoistettuValintalaskennanTulosDTO poistetut =
        findPoistetutValintalaskennanTulokset(startDatetime, endDatatime);
    if (!poistetut.getPoistetut().isEmpty()) {
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForTulosdata(
              poistetut.getPoistetut(),
              "valintalaskennan_tulos",
              opId,
              siirtotiedostoKeys.size() + 1));
    }
    tulosCount.addAndGet(poistetut.getPoistetut().size());
    LOGGER.info(
        "Kirjoitettiin yhteensä {} valintalaskennan tulosta {} siirtotiedostoon.",
        tulosCount.get(),
        siirtotiedostoKeys.size());
    return resultJson(siirtotiedostoKeys, tulosCount.intValue());
  }

  private PoistettuValintalaskennanTulosDTO findPoistetutValintalaskennanTulokset(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    List<Poistettu> poistetutValinnanvaiheet =
        poistettuDAO.findPoistetut(VALINNANVAIHE, startDatetime, endDatatime);
    List<Poistettu> poistetutValintatapaJonot =
        poistettuDAO.findPoistetut(VALINTATAPAJONO, startDatetime, endDatatime);
    List<Poistettu> poistetutJonosijat =
        poistettuDAO.findPoistetut(JONOSIJA, startDatetime, endDatatime);

    List<UUID> requiredParentValintatapaJonot =
        new ArrayList<>(poistetutJonosijat.stream().map(Poistettu::getParentId).toList());
    List<Poistettu> valintatapaJonot =
        findAllRequiredParents(
            VALINTATAPAJONO, poistetutValintatapaJonot, requiredParentValintatapaJonot);

    List<UUID> requiredParentValinnanvaiheet =
        new ArrayList<>(valintatapaJonot.stream().map(Poistettu::getParentId).toList());
    List<Poistettu> valinnanVaiheet =
        findAllRequiredParents(
            VALINNANVAIHE, poistetutValinnanvaiheet, requiredParentValinnanvaiheet);

    List<ValinnanVaihe> poistetutVaiheet =
        valinnanVaiheet.stream()
            .map(
                vaihe -> {
                  ValinnanVaihe valinnanVaihe =
                      new ValinnanVaihe().setValinnanvaiheoid(vaihe.getTunniste());
                  valinnanVaihe.setPoistettuItself(vaihe.isDeletedItself());
                  if (!vaihe.isDeletedItself()) {
                    List<Poistettu> childJonot = findChildren(valintatapaJonot, vaihe);
                    List<ValintatapaJono> poistetutJonot =
                        childJonot.stream()
                            .map(
                                jono -> {
                                  ValintatapaJono valintatapaJono =
                                      new ValintatapaJono()
                                          .setValintatapajonooid(jono.getTunniste());
                                  valintatapaJono.setPoistettuItself(jono.isDeletedItself());
                                  if (!jono.isDeletedItself()) {
                                    List<Poistettu> childJonosijat =
                                        findChildren(poistetutJonosijat, jono);
                                    List<Jonosija> jonosijat =
                                        childJonosijat.stream()
                                            .map(
                                                sija ->
                                                    new Jonosija()
                                                        .setHakemusOid(sija.getTunniste()))
                                            .toList();
                                    valintatapaJono.setJonosijat(
                                        jonosijat.isEmpty() ? null : jonosijat);
                                  }
                                  return valintatapaJono;
                                })
                            .toList();
                    valinnanVaihe.setValintatapajonot(
                        poistetutJonot.isEmpty() ? null : poistetutJonot);
                  }
                  return valinnanVaihe;
                })
            .toList();
    return new PoistettuValintalaskennanTulosDTO().setPoistetut(poistetutVaiheet);
  }

  private String resultJson(List<String> siirtotiedostoKeys, int itemCount) {
    JsonArray keyJson = new JsonArray();
    siirtotiedostoKeys.forEach(key -> keyJson.add(key));
    JsonObject result = new JsonObject();
    result.add("keys", keyJson);
    result.addProperty("total", itemCount);
    result.addProperty("success", true);
    return result.toString();
  }
}
