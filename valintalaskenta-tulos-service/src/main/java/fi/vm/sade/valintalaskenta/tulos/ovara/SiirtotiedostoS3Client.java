package fi.vm.sade.valintalaskenta.tulos.ovara;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonWriter;
import fi.vm.sade.valinta.dokumenttipalvelu.SiirtotiedostoPalvelu;
import fi.vm.sade.valinta.dokumenttipalvelu.dto.ObjectMetadata;
import java.io.*;
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
  private static final Gson gson =
      new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").create();

  private final SiirtotiedostoPalvelu siirtotiedostoPalvelu;
  private final int maxValinnanvaiheCountInFile;
  private final int maxHakemusCountInFile;

  @Autowired
  public SiirtotiedostoS3Client(
      @Value("${aws.region}") final String awsRegion,
      @Value("${aws.siirtotiedosto.bucket-name}") final String s3Bucket,
      @Value("${aws.siirtotiedosto.bucket-target-role-arn}") final String s3TargetRoleArn,
      @Value("${aws.siirtotiedosto.max-valinnanvaihe-count-in-file}")
          final int maxValinnanvaiheCountInFile,
      @Value("${aws.siirtotiedosto.max-hakemus-count-in-file}") final int maxHakemusCountInFile) {
    this.siirtotiedostoPalvelu = new SiirtotiedostoPalvelu(awsRegion, s3Bucket, s3TargetRoleArn);
    this.maxValinnanvaiheCountInFile = maxValinnanvaiheCountInFile;
    this.maxHakemusCountInFile = maxHakemusCountInFile;
  }

  public String createSiirtotiedostoForTulosdata(
      List<?> data, String dataType, String opId, int opSubId) {
    try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
      try (OutputStreamWriter outputStreamWriter = new OutputStreamWriter(outputStream)) {
        JsonWriter jsonWriter = new JsonWriter(outputStreamWriter);
        jsonWriter.beginArray();
        for (Object dto : data) {
          gson.toJson(gson.toJsonTree(dto), jsonWriter);
        }
        jsonWriter.endArray();
        jsonWriter.close();

        try (ByteArrayInputStream inputStream =
            new ByteArrayInputStream(outputStream.toByteArray())) {
          return doCreateSiirtotiedosto(inputStream, dataType, opId, opSubId);
        }
      }
    } catch (IOException ioe) {
      throw new RuntimeException("JSONin muodostaminen epäonnistui;", ioe);
    }
  }

  private String doCreateSiirtotiedosto(
      InputStream inputStream, String dataType, String opId, int opSubId) {
    try {
      ObjectMetadata result =
          siirtotiedostoPalvelu.saveSiirtotiedosto(
              "valintalaskenta", dataType, "", opId, opSubId, inputStream, 2);
      return result.key;
    } catch (Exception e) {
      logger.error("Siirtotiedoston luonti epäonnistui; ", e);
      throw new RuntimeException(e);
    }
  }

  public int getMaxValinnanvaiheCountInFile() {
    return this.maxValinnanvaiheCountInFile;
  }

  public int getMaxHakemusCountInFile() {
    return this.maxHakemusCountInFile;
  }
}
