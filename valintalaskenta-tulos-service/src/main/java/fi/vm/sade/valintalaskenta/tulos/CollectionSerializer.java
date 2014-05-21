package fi.vm.sade.valintalaskenta.tulos;

import org.codehaus.jackson.JsonGenerator;
import org.codehaus.jackson.JsonProcessingException;
import org.codehaus.jackson.map.JsonSerializer;
import org.codehaus.jackson.map.SerializerProvider;

import java.io.IOException;
import java.util.Collection;

/**
 * User: bleed
 * Date: 3/7/13
 * Time: 4:17 PM
 */
public class CollectionSerializer extends JsonSerializer<Collection> {
    @Override
    public void serialize(Collection value, JsonGenerator jgen, SerializerProvider provider) throws IOException, JsonProcessingException {
        if(value == null) {
            jgen.writeNull();
            return;
        }

        jgen.writeStartArray();
        for(Object obj : value) {
            JsonSerializer<Object> valueSerializer = provider.findValueSerializer(obj.getClass(), null);
            valueSerializer.serialize(obj, jgen, provider);
        }
        jgen.writeEndArray();
    }
}
