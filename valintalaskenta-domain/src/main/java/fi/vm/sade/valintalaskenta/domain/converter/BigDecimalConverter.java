package fi.vm.sade.valintalaskenta.domain.converter;

import org.mongodb.morphia.converters.SimpleValueConverter;
import org.mongodb.morphia.converters.TypeConverter;
import org.mongodb.morphia.mapping.MappedField;
import org.mongodb.morphia.mapping.MappingException;

import java.math.BigDecimal;

/**
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
