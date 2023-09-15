package fi.vm.sade.valintalaskenta.laskenta.dao;

import static org.jooq.impl.DSL.*;

import org.hibernate.Session;
import org.hibernate.jdbc.Work;
import org.jooq.*;
import org.jooq.impl.*;

import javax.persistence.EntityManager;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public final class QueryUtil {



    public static <E> List<E> nativeQuery(EntityManager em, org.jooq.Query query, Class<E> type) {

        // Extract the SQL statement from the jOOQ query:
        javax.persistence.Query result = em.createNativeQuery(query.getSQL(), type);

        // Extract the bind values from the jOOQ query:
        List<Object> values = query.getBindValues();
        for (int i = 0; i < values.size(); i++) {
            System.out.println(values.get(i));
            result.setParameter(i + 1, values.get(i));
        }

        // There's an unsafe cast here, but we can be sure that we'll get the right type from JPA
        return result.getResultList();
    }

    public static <E> List<E> jooqQuery(EntityManager em, Function<DSLContext, org.jooq.Query> consumer, Class<E> type) {
        Session session = em.unwrap(Session.class);
        List<E> result = new ArrayList<>();
        session.doWork(connection -> result.addAll(nativeQuery(em, consumer.apply(DSL.using(connection)), type)));
        return result;
    }

}
