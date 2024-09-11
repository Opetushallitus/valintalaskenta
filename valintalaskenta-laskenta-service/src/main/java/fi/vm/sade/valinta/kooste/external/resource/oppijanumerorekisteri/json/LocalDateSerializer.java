package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.json;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;

public class LocalDateSerializer extends JsonSerializer {
  @Override
  public void serialize(Object value, JsonGenerator gen, SerializerProvider serializers)
      throws IOException, JsonProcessingException {
    if (value != null) {
      gen.writeString(value.toString());
    } else {
      gen.writeNull();
    }
  }
}
