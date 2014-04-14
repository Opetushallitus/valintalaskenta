package fi.vm.sade.valintalaskenta.domain.converter;

import com.google.code.morphia.converters.SimpleValueConverter;
import com.google.code.morphia.converters.TypeConverter;
import com.google.code.morphia.mapping.MappedField;
import com.google.code.morphia.mapping.MappingException;

import java.math.BigDecimal;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         https://code.google.com/p/morphia/issues/detail?id=411
 *         https://code.google.com/p/morphia/issues/detail?id=412
 */
public class BigDecimalConverter extends TypeConverter implements SimpleValueConverter {

    public BigDecimalConverter() {
        super(BigDecimal.class);
    }

    @Override
    public Object encode(Object value, MappedField optionalExtraInfo) {
        if (value == null) {
            return "";
        } else {
            return value.toString();
        }
    }

    @Override
    public Object decode(Class targetClass, Object fromDBObject, MappedField optionalExtraInfo) throws MappingException {
        if (fromDBObject == null || fromDBObject.toString().isEmpty()) {
            return null;
        }
        return new BigDecimal(fromDBObject.toString());
    }

}
