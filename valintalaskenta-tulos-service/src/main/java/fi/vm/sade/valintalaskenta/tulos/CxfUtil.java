package fi.vm.sade.valintalaskenta.tulos;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import fi.vm.sade.javautils.legacy_cxf_cas.authentication.cas.CasApplicationAsAUserInterceptor;
import java.util.List;
import javax.ws.rs.ext.ContextResolver;
import org.apache.cxf.interceptor.Interceptor;
import org.apache.cxf.jaxrs.client.ClientConfiguration;
import org.apache.cxf.jaxrs.client.JAXRSClientFactoryBean;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.message.Message;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.ConnectionType;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;

public class CxfUtil {
  public static CasApplicationAsAUserInterceptor casInterceptor(
      final String casUrl, final String targetUrl, final String username, final String password) {
    final CasApplicationAsAUserInterceptor interceptor = new CasApplicationAsAUserInterceptor();
    interceptor.setWebCasUrl(casUrl);
    interceptor.setTargetService(targetUrl);
    interceptor.setAppClientUsername(username);
    interceptor.setAppClientPassword(password);
    return interceptor;
  }

  public static <T> T createClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ContextResolver<ObjectMapper> objectMapperProvider,
      final String address,
      final Interceptor<Message> ophRequestHeaders,
      final CasApplicationAsAUserInterceptor casInterceptor,
      final Class<T> cls) {
    // Create rest client
    final JAXRSClientFactoryBean bean = new JAXRSClientFactoryBean();
    bean.setInheritHeaders(true);
    bean.setAddress(address);
    bean.setProviders(List.of(jacksonJsonProvider, objectMapperProvider));
    bean.setOutInterceptors(List.of(ophRequestHeaders, casInterceptor));
    bean.setResourceClass(cls);

    return bean.create(cls);
  }

  public static <T> T createClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ContextResolver<ObjectMapper> objectMapperProvider,
      final String address,
      final Interceptor<Message> ophRequestHeaders,
      final CasApplicationAsAUserInterceptor casInterceptor,
      final Class<T> cls,
      final long clientConnectionTimeout,
      final long clientReceiveTimeout) {
    final T restClient =
        createClient(
            jacksonJsonProvider,
            objectMapperProvider,
            address,
            ophRequestHeaders,
            casInterceptor,
            cls);

    // Set http conduit settings
    final HTTPClientPolicy policy = new HTTPClientPolicy();
    policy.setConnection(ConnectionType.KEEP_ALIVE);
    policy.setConnectionTimeout(clientConnectionTimeout);
    policy.setReceiveTimeout(clientReceiveTimeout);
    policy.setAllowChunking(false);
    final ClientConfiguration client = WebClient.getConfig(restClient);
    final HTTPConduit conduit = (HTTPConduit) client.getConduit();
    conduit.setClient(policy);

    return restClient;
  }
}
