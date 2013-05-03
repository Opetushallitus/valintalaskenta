package fi.vm.sade.valintalaskenta;

/**
 * User: kwuoti
 * Date: 13.3.2013
 * Time: 14.09
 */

import javax.ws.rs.ext.ContextResolver;
import javax.ws.rs.ext.Provider;

import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.springframework.stereotype.Component;

/**
 * 
 * @author Jussi Jartamo
 * 
 * @Deprecated Asenna selaimeen esimerkiksi JSONView liitännäinen niin ei
 *             tarvitse ohjelmallisesti PrettyPrintata JSON:ia.
 * 
 */
@Deprecated
@Component
@Provider
public class ObjectMapperProvider implements ContextResolver<ObjectMapper> {

    private final ObjectMapper objectMapper;

    public ObjectMapperProvider() {
        objectMapper = new ObjectMapper();

        objectMapper.configure(SerializationConfig.Feature.DEFAULT_VIEW_INCLUSION, false);
        objectMapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        // FIXME: Tämä tulostaa jsonin nätimmässä muodossa. Varmaan pois
        // tuotannosta..
        objectMapper.configure(SerializationConfig.Feature.INDENT_OUTPUT, true);
    }

    @Override
    public ObjectMapper getContext(Class<?> type) {
        return objectMapper;
    }
}
