package fi.vm.sade.valintalaskenta.laskenta;

/**
 * Created with IntelliJ IDEA.
 * User: jukais
 * Date: 16.1.2013
 * Time: 18.03
 * To change this template use File | Settings | File Templates.
 */

import org.codehaus.jackson.Version;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.module.SimpleModule;
import org.springframework.stereotype.Component;

import javax.ws.rs.ext.ContextResolver;
import javax.ws.rs.ext.Provider;
import java.util.Collection;

@Component
@Provider
public class ObjectMapperProvider implements ContextResolver<ObjectMapper> {

    private final ObjectMapper objectMapper;

    public ObjectMapperProvider() {
        objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        //objectMapper.setSerializerFactory(new HibernateAwareSerializerFactory(null));
        SimpleModule module = new SimpleModule("Module", new Version(1, 0, 0, null));
        module.addSerializer(Collection.class, new CollectionSerializer());
        objectMapper.registerModule(module);
    }

    @Override
    public ObjectMapper getContext(Class<?> type) {
        return objectMapper;
    }
}
