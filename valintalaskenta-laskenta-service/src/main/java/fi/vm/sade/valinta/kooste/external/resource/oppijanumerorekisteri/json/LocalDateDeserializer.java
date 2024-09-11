package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.json;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;

public class LocalDateDeserializer extends JsonDeserializer {
  @Override
  public Object deserialize(JsonParser p, DeserializationContext ctxt)
      throws IOException, JsonProcessingException {
    if (!p.hasToken(JsonToken.VALUE_STRING)) {
      throw ctxt.wrongTokenException(p, JsonToken.VALUE_STRING, null);
    }
    String string = p.getText().trim();
    if (string.isEmpty()) {
      return null;
    }
    try {
      return LocalDate.parse(string);
    } catch (DateTimeParseException e1) {
      JsonMappingException e2 = ctxt.weirdStringException(string, handledType(), e1.getMessage());
      e2.initCause(e1);
      throw e2;
    }
  }
}
