package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

import fi.vm.sade.tarjonta.service.resources.v1.dto.HakuV1RDTO;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto.KoutaHaku;
import java.util.*;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class Haku {
  public final String oid;
  public final Map<String, String> nimi;
  public final Set<String> tarjoajaOids;
  public final String kohdejoukkoUri;
  public final String hakukausiUri;
  public final Integer hakukausiVuosi;
  public final String koulutuksenAlkamiskausiUri;
  public final Integer koulutuksenAlkamisvuosi;
  public final String ataruLomakeAvain;

  public Haku(
      String oid,
      Map<String, String> nimi,
      Set<String> tarjoajaOids,
      String ataruLomakeAvain,
      String kohdejoukkoUri,
      String hakukausiUri,
      Integer hakukausiVuosi,
      String koulutuksenAlkamiskausiUri,
      Integer koulutuksenAlkamisvuosi) {
    this.oid = oid;
    this.nimi = nimi;
    this.tarjoajaOids = tarjoajaOids;
    this.kohdejoukkoUri = kohdejoukkoUri;
    this.hakukausiUri = hakukausiUri;
    this.hakukausiVuosi = hakukausiVuosi;
    this.koulutuksenAlkamiskausiUri = koulutuksenAlkamiskausiUri;
    this.koulutuksenAlkamisvuosi = koulutuksenAlkamisvuosi;
    this.ataruLomakeAvain = ataruLomakeAvain;
  }

  public Haku(HakuV1RDTO dto) {
    this.oid = dto.getOid();
    this.nimi = dto.getNimi();
    this.tarjoajaOids = new HashSet<>(Arrays.asList(dto.getTarjoajaOids()));
    this.kohdejoukkoUri = dto.getKohdejoukkoUri();
    this.hakukausiUri = dto.getHakukausiUri();
    this.hakukausiVuosi = dto.getHakukausiVuosi();
    this.koulutuksenAlkamiskausiUri = dto.getKoulutuksenAlkamiskausiUri();
    this.koulutuksenAlkamisvuosi = dto.getKoulutuksenAlkamisVuosi();
    this.ataruLomakeAvain = dto.getAtaruLomakeAvain();
  }

  public Haku(KoutaHaku dto) {

    DateTimeFormatter fmt = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss");

    this.oid = dto.oid;
    this.nimi = new HashMap<>();
    dto.nimi.forEach((kieli, nimi) -> this.nimi.put("kieli_" + kieli, nimi));
    this.tarjoajaOids = new HashSet<>();
    this.tarjoajaOids.add(dto.organisaatioOid);
    this.kohdejoukkoUri = dto.kohdejoukkoKoodiUri;

    if (dto.hakuajat != null && !dto.hakuajat.isEmpty()) {
      OptionalInt tuorein =
          dto.hakuajat.stream()
              .map(kh -> Arrays.asList(kh.alkaa, kh.paattyy))
              .flatMap(Collection::stream)
              .filter(Objects::nonNull)
              .map(aikaleima -> DateTime.parse(aikaleima, fmt).getYear())
              .mapToInt(v -> v)
              .max();
      if (tuorein.isPresent()) {
        this.hakukausiVuosi = tuorein.getAsInt();
      } else {
        this.hakukausiVuosi = null;
      }
    } else {
      this.hakukausiVuosi = null;
    }
    this.hakukausiUri = null; // TODO

    this.koulutuksenAlkamiskausiUri = dto.alkamiskausiKoodiUri;
    this.koulutuksenAlkamisvuosi =
        dto.alkamisvuosi != null ? Integer.parseInt(dto.alkamisvuosi) : null;
    this.ataruLomakeAvain = dto.hakulomakeAtaruId;
  }
}
