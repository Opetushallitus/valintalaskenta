package fi.vm.sade.valintalaskenta.tulos;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import fi.vm.sade.valinta.dokumenttipalvelu.SiirtotiedostoPalvelu;
import fi.vm.sade.valinta.dokumenttipalvelu.dto.ObjectMetadata;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import java.io.ByteArrayInputStream;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class SiirtotiedostoS3Client {
  private static final Logger logger =
      LoggerFactory.getLogger(SiirtotiedostoS3Client.class.getName());
  private static final Gson gson = new GsonBuilder().create();

  private final SiirtotiedostoPalvelu siirtotiedostoPalvelu;
  private final int maxHakukohdeCountInFile;
  private final int maxHakemusCountInFile;

  @Autowired
  public SiirtotiedostoS3Client(
      @Value("${aws.region}") final String awsRegion,
      @Value("${aws.siirtotiedosto.bucket-name}") final String s3Bucket,
      @Value("${aws.siirtotiedosto.bucket-target-role-arn}") final String s3TargetRoleArn,
      @Value("${aws.siirtotiedosto.max-hakukohde-count-in-file}") final int maxHakukohdeCountInFile,
      @Value("${aws.siirtotiedosto.max-hakemus-count-in-file}") final int maxHakemusCountInFile) {
    this.siirtotiedostoPalvelu = new SiirtotiedostoPalvelu(awsRegion, s3Bucket, s3TargetRoleArn);
    this.maxHakukohdeCountInFile = maxHakukohdeCountInFile;
    this.maxHakemusCountInFile = maxHakemusCountInFile;
  }

  public String createSiirtotiedostoForValintakoeOsallistumiset(
      List<ValintakoeOsallistuminen> data) {
    JsonArray jsonArray = new JsonArray(data.size());
    data.forEach(item -> jsonArray.add(gson.toJsonTree(item)));
    return doCreateSiirtotiedosto(jsonArray, "valintakoe_osallistuminen");
  }

  public String createSiirtotiedostoForValintalaskennanTulokset(
      List<ValintatietoValinnanvaiheDTO> data) {
    JsonArray jsonArray = new JsonArray(data.size());
    data.forEach(item -> jsonArray.add(gson.toJsonTree(item)));
    return doCreateSiirtotiedosto(jsonArray, "valintalaskennan_tulos");
  }

  private String doCreateSiirtotiedosto(JsonArray jsonArray, String dataType) {
    try {
      ObjectMetadata result =
          siirtotiedostoPalvelu.saveSiirtotiedosto(
              "valintalaskenta",
              dataType,
              "",
              new ByteArrayInputStream(jsonArray.toString().getBytes()),
              2);
      return result.key;
    } catch (Exception e) {
      logger.error("Siirtotiedoston luonti epäonnistui; ", e);
      throw new RuntimeException(e);
    }
  }

  public int getMaxHakukohdeCountInFile() {
    return this.maxHakukohdeCountInFile;
  }

  public int getMaxHakemusCountInFile() {
    return this.maxHakemusCountInFile;
  }
}
