package fi.vm.sade.security;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.methods.GetMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;

/**
 * @author Antti Salonen
 */
@Component
public class OidProvider {

    protected final Logger log = LoggerFactory.getLogger(getClass());

    @Value("${cas.service.organisaatio-service}")
    private String organisaatioServiceUrl;

    @Value("${root.organisaatio.oid}")
    private String rootOrganisaatioOid;

    public OidProvider() {
    }

    public OidProvider(String organisaatioServiceUrl) {
        this.organisaatioServiceUrl = organisaatioServiceUrl;
    }

    public List<String> getSelfAndParentOids(String organisaatioOid) {
        try {
            String url = organisaatioServiceUrl+"/rest/organisaatio/"+organisaatioOid+"/parentoids";
            String result = httpGet(url, 200);
            return Arrays.asList(result.split("/"));
        } catch (Exception e) {
            log.warn("failed to getSelfAndParentOids, exception: "+e+", returning only rootOrganisaatioOid and organisaatioOid");
            return Arrays.asList(rootOrganisaatioOid, organisaatioOid);
        }
    }

    private String httpGet(String url, int expectedStatus) {
        HttpClient client = new HttpClient();
        GetMethod get = new GetMethod(url);
        try {
            client.executeMethod(get);
            final String response = get.getResponseBodyAsString();
            if (get.getStatusCode() == expectedStatus) {
                return response;
            } else {
                throw new RuntimeException("failed to call '"+url+"', invalid status: "+get.getStatusCode()+"/"+get.getStatusText());
            }
        } catch (final Exception e) {
            throw new RuntimeException("failed to call '"+url+"': "+e, e);
        } finally {
            get.releaseConnection();
        }
    }

    public void setOrganisaatioServiceUrl(String organisaatioServiceUrl) {
        this.organisaatioServiceUrl = organisaatioServiceUrl;
    }
}
