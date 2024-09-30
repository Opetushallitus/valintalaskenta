package fi.vm.sade.valinta.kooste.valintalaskenta.service;

import fi.vm.sade.service.valintaperusteet.dto.HakukohdeViiteDTO;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Maski;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class HakukohdeService {
  private static final Logger LOG = LoggerFactory.getLogger(HakukohdeService.class);

  private final ValintaperusteetAsyncResource valintaperusteetAsyncResource;

  @Autowired
  public HakukohdeService(
      ValintaperusteetAsyncResource valintaperusteetAsyncResource) {
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
  }

  public Collection<HakukohdeJaOrganisaatio> fetchHakukohteet(final LaskentaDto laskenta) {
    String hakuOid = laskenta.getHakuOid();
    if (StringUtils.isBlank(hakuOid)) {
      LOG.error("Yritettiin hakea hakukohteita ilman hakuOidia!");
      throw new RuntimeException("Yritettiin hakea hakukohteita ilman hakuOidia!");
    }

    CompletableFuture<List<HakukohdeViiteDTO>> hakukohteet = valintaperusteetAsyncResource.haunHakukohteet(hakuOid);
    return maskHakukohteet(hakuOid, hakukohteet.join(), laskenta);
  }

  private static Collection<HakukohdeJaOrganisaatio> maskHakukohteet(
      String hakuOid, List<HakukohdeViiteDTO> hakukohdeViitteet, LaskentaDto laskenta) {
    LOG.info("Tarkastellaan hakukohdeviitteita haulle {}", hakuOid);

    final List<HakukohdeJaOrganisaatio> haunHakukohdeOidit =
        hakukohdeViitteet != null
            ? publishedNonNulltoHakukohdeJaOrganisaatio(hakukohdeViitteet)
            : new ArrayList<>();
    final Maski maski = createMaskiFromLaskenta(laskenta);

    return maski.maskaa(haunHakukohdeOidit);
  }

  private static List<HakukohdeJaOrganisaatio> publishedNonNulltoHakukohdeJaOrganisaatio(
      final List<HakukohdeViiteDTO> hakukohdeViitteet) {
    return hakukohdeViitteet.stream()
        .filter(Objects::nonNull)
        .filter(h -> h.getOid() != null)
        .filter(h -> h.getTila().equals("JULKAISTU"))
        .map(h -> new HakukohdeJaOrganisaatio(h.getOid(), h.getTarjoajaOid()))
        .collect(Collectors.toList());
  }

  private static Maski createMaskiFromLaskenta(final LaskentaDto laskenta) {
    final List<String> hakukohdeOids =
        laskenta.getHakukohteet().stream()
            .filter(h -> !HakukohdeTila.VALMIS.equals(h.getTila()))
            .map(h -> new HakukohdeJaOrganisaatio(h.getHakukohdeOid(), h.getOrganisaatioOid()))
            .map(HakukohdeJaOrganisaatio::getHakukohdeOid)
            .collect(Collectors.toList());

    return Maski.whitelist(hakukohdeOids);
  }
}
