package fi.vm.sade.valintalaskenta.laskenta;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import java.io.IOException;
import java.util.Collection;

public class CollectionSerializer extends JsonSerializer<Collection> {
    @Override
    public void serialize(Collection value, JsonGenerator jgen, SerializerProvider provider) throws IOException {
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
