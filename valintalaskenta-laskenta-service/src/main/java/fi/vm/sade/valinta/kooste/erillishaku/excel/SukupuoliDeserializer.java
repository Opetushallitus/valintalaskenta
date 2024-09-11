package fi.vm.sade.valinta.kooste.erillishaku.excel;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;

/** Koska SUKUPUOLI ottaa vastaan myös arvot 1 ja 2 */
public class SukupuoliDeserializer extends JsonDeserializer<Sukupuoli> {

  @Override
  public Sukupuoli deserialize(JsonParser jsonParser, DeserializationContext deserializationContext)
      throws IOException, JsonProcessingException {
    return Sukupuoli.fromString(jsonParser.getValueAsString());
  }
}
