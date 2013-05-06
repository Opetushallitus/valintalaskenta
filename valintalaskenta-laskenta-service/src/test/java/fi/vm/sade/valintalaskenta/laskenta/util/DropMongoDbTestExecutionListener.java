package fi.vm.sade.valintalaskenta.laskenta.util;

import com.google.code.morphia.Datastore;
import org.springframework.test.context.TestContext;
import org.springframework.test.context.support.AbstractTestExecutionListener;

/**
* User: wuoti
* Date: 29.4.2013
* Time: 10.44
*/
public class DropMongoDbTestExecutionListener extends AbstractTestExecutionListener {

    @Override
    public void afterTestMethod(TestContext testContext) throws Exception {
        Datastore datastore = testContext.getApplicationContext().getBean(Datastore.class);
        datastore.getMongo().dropDatabase(datastore.getDB().getName());
    }
}
