package fi.vm.sade.valintalaskenta.laskenta.config;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import fi.vm.sade.valintalaskenta.domain.valinta.FunktioTulosContainer;
import fi.vm.sade.valintalaskenta.domain.valinta.SyotettyArvoContainer;
import org.postgresql.util.PGobject;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.TypeDescriptor;
import org.springframework.core.convert.converter.GenericConverter;
import org.springframework.data.convert.ReadingConverter;
import org.springframework.data.convert.WritingConverter;
import org.springframework.data.jdbc.core.convert.JdbcCustomConversions;
import org.springframework.data.jdbc.repository.config.AbstractJdbcConfiguration;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.TransactionManager;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static java.util.Arrays.asList;

@Configuration
@EnableJdbcRepositories(basePackages = {"fi.vm.sade.valintalaskenta.laskenta.dao.repository"})
class DatabaseConfiguration extends AbstractJdbcConfiguration {

    private final ApplicationContext applicationContext;

    private final static List<Class<?>> JSON_CLASSES = asList(SyotettyArvoContainer.class, FunktioTulosContainer.class);

    DatabaseConfiguration(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    @Bean
    NamedParameterJdbcOperations namedParameterJdbcOperations(DataSource dataSource) {
        return new NamedParameterJdbcTemplate(dataSource);
    }

    @Bean
    TransactionManager transactionManager(DataSource dataSource) {
        return new DataSourceTransactionManager(dataSource);
    }

    @Override
    @Bean
    public JdbcCustomConversions jdbcCustomConversions() {
        List<GenericConverter> converters = new ArrayList<>();
        JSON_CLASSES.forEach(clazz -> {
            converters.add(new ObjectToJSONB<>(clazz));
            converters.add(new JSONBToObject<>(clazz));
        });
        return new JdbcCustomConversions(converters);
    }

    @WritingConverter
    class ObjectToJSONB<S> implements GenericConverter {

        private final Class<S> sourceClazz;

        private final ObjectMapper mapper;

        ObjectToJSONB(Class<S> sourceClazz) {
            this.sourceClazz = sourceClazz;
            mapper = applicationContext.getBean(ObjectMapper.class);
        }

        @Override
        public Set<ConvertiblePair> getConvertibleTypes() {
            return Collections.singleton(new GenericConverter.ConvertiblePair(sourceClazz, PGobject.class));
        }

        @Override
        public Object convert(Object source, TypeDescriptor sourceType, TypeDescriptor targetType) {
            Object sourceObject = sourceClazz.cast(source);
            PGobject pgJsonObject = new PGobject();
            pgJsonObject.setType("jsonb");
            try {
                pgJsonObject.setValue(mapper.writeValueAsString(sourceObject));
            } catch (SQLException | JsonProcessingException e) {
                throw new RuntimeException(e);
            }
            return pgJsonObject;
        }
    }

    @ReadingConverter
    class JSONBToObject<S> implements GenericConverter {
        private final Class<S> targetClazz;

        private final ObjectMapper mapper;

        JSONBToObject(Class<S> targetClazz) {
            this.targetClazz = targetClazz;
            mapper = applicationContext.getBean(ObjectMapper.class);
        }

        @Override
        public Set<ConvertiblePair> getConvertibleTypes() {
            return Collections.singleton(new GenericConverter.ConvertiblePair(PGobject.class, targetClazz));
        }

        @Override
        public Object convert(Object source, TypeDescriptor sourceType, TypeDescriptor targetType) {
            PGobject pgObject = (PGobject) source;
            try {
                return mapper.readValue(pgObject.getValue(), targetClazz);
            } catch (JsonProcessingException e) {
                throw new RuntimeException(e);
            }
        }
    }

}