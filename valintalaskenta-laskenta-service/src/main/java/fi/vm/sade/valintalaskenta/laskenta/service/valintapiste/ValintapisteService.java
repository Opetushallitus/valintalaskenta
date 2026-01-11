package fi.vm.sade.valintalaskenta.laskenta.service.valintapiste;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Pistetieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.PistetietoWrapper;
import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.AtaruHakemus;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.AtaruResource;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintapisteDAO;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ValintapisteService {
  private static final Logger LOG = LoggerFactory.getLogger(ValintapisteService.class);

  private final ValintapisteDAO valintapisteDAO;
  private final AtaruResource ataruResource;

  @Autowired
  ValintapisteService(ValintapisteDAO dao, AtaruResource ataruResource) {
    this.valintapisteDAO = dao;
    this.ataruResource = ataruResource;
  }

  @Transactional(readOnly = true)
  public Pair<ZonedDateTime, PistetietoWrapper> findValintapisteetForHakemus(
      String hakemusOid, String oppijaId) {
    Pair<ZonedDateTime, List<PistetietoWrapper>> result =
        findValintapisteetForHakemukset(List.of(hakemusOid));
    return Pair.of(result.getLeft(), result.getRight().get(0).withOppijaOID(oppijaId));
  }

  @Transactional(readOnly = true)
  public Pair<ZonedDateTime, List<PistetietoWrapper>> findValintapisteetForHakemukset(
      Collection<String> hakemusOids) {
    if (hakemusOids.size() > 32767) {
      throw new IllegalArgumentException(
          "Pisteitä voi hakea korkeintaan 32767 hakemukselle kerralla. Nyt oli "
              + hakemusOids.size());
    }
    if (hakemusOids.isEmpty()) {
      return Pair.of(null, List.of());
    }

    Map<String, List<Pistetieto>> pisteetByHakemukset =
        groupByHakemus(valintapisteDAO.findValintapisteetForHakemukset(hakemusOids));

    Stream<PistetietoWrapper> missingHakemusOids =
        hakemusOids.stream()
            .filter(oid -> !pisteetByHakemukset.containsKey(oid))
            .map(PistetietoWrapper::new);

    List<PistetietoWrapper> pisteet =
        Stream.concat(convertToWrappers(pisteetByHakemukset), missingHakemusOids).toList();

    Optional<ZonedDateTime> lastModified = lastModifiedForHakemukset(hakemusOids);

    return Pair.of(lastModified.orElse(null), pisteet);
  }

  @Transactional(readOnly = true)
  public Optional<ZonedDateTime> lastModifiedForHakemukset(Collection<String> hakemusOids) {
    // Tietokannan aikaleima on tarkempi kuin Javan. Lisätään aikaan sekunti, jotta palautetun
    // leiman uudelleenkäyttö ei aiheuta virheitä kutsujalle.
    return valintapisteDAO
        .lastModifiedForHakemukset(hakemusOids)
        .map(a -> a.withZoneSameInstant(ZoneId.of("Europe/Helsinki")).plusSeconds(1));
  }

  @Transactional
  public List<String> insertValintapisteet(
      List<PistetietoWrapper> pisteet, boolean savePartially, String ifUnmodifiedSince) {
    LOG.info("Halutaan lisätä pistetietoja {} hakemukselle.", pisteet.size());
    List<String> hakemusOids = pisteet.stream().map(PistetietoWrapper::hakemusOID).toList();
    List<String> conflictingOids = checkUpdateConflict(hakemusOids, ifUnmodifiedSince);

    if (conflictingOids.isEmpty() || savePartially) {
      if (!conflictingOids.isEmpty()) {
        LOG.info("Päivitetään ne pistetiedot, jotka eivät ole konfliktissa.");
      }
      pisteet.stream()
          .filter(wrapper -> !conflictingOids.contains(wrapper.hakemusOID()))
          .flatMap(this::createValintapisteet)
          .forEach(valintapisteDAO::upsertValintapiste);
    } else {
      LOG.warn("Hakemusten pistetietoja ei päivitetty päivityskonfliktin takia.");
    }

    return conflictingOids;
  }

  @Transactional(readOnly = true)
  public Pair<ZonedDateTime, List<PistetietoWrapper>> hakukohteenValintapisteet(
      String hakuOid, String hakukohdeOid) {
    Map<String, String> hakemukset =
        ataruResource.getHakemukset(hakuOid, hakukohdeOid).stream()
            .collect(Collectors.toMap(AtaruHakemus::hakemusOid, AtaruHakemus::henkiloOid));
    Pair<ZonedDateTime, List<PistetietoWrapper>> response =
        findValintapisteetForHakemukset(hakemukset.keySet());
    List<PistetietoWrapper> pisteet =
        response.getRight().stream()
            .map(wrapper -> wrapper.withOppijaOID(hakemukset.get(wrapper.hakemusOID())))
            .toList();
    return Pair.of(response.getLeft(), pisteet);
  }

  private Stream<Valintapiste> createValintapisteet(PistetietoWrapper wrapper) {
    return wrapper.pisteet().stream().map(piste -> createValintapiste(piste, wrapper.hakemusOID()));
  }

  private Valintapiste createValintapiste(Pistetieto pistetieto, String hakemusOid) {
    return new Valintapiste(
        hakemusOid,
        pistetieto.tunniste(),
        pistetieto.arvo(),
        pistetieto.osallistuminen(),
        pistetieto.tallettaja());
  }

  private List<String> checkUpdateConflict(List<String> hakemusOids, String unmodifiedSince) {
    if (unmodifiedSince == null) {
      return List.of();
    }
    List<String> conflictingOids =
        valintapisteDAO.modifiedSinceHakemukset(hakemusOids, unmodifiedSince);
    if (!conflictingOids.isEmpty()) {
      LOG.info(
          "Ainakin osa hakemuksista on päivityskonflitissa: {}",
          String.join(", ", conflictingOids));
    }
    return conflictingOids;
  }

  public static Map<String, List<Pistetieto>> groupByHakemus(List<Valintapiste> valintapisteet) {
    return valintapisteet.stream()
        .collect(
            Collectors.groupingBy(
                Valintapiste::hakemusOid,
                Collectors.mapping(Valintapiste::toPistetieto, Collectors.toList())));
  }

  public static Stream<PistetietoWrapper> convertToWrappers(Map<String, List<Pistetieto>> pisteet) {
    return pisteet.entrySet().stream().map(kv -> new PistetietoWrapper(kv.getKey(), kv.getValue()));
  }
}
