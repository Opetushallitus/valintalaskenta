package fi.vm.sade.valintalaskenta.tulos;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializer;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.javautils.nio.cas.CasClient;
import java.util.Date;
import java.util.List;
import java.util.Map;
import org.asynchttpclient.Request;
import org.asynchttpclient.RequestBuilder;
import org.asynchttpclient.Response;

public class RestClientUtil {
  public static final Gson GSON =
      new GsonBuilder()
          .registerTypeAdapter(
              Date.class,
              (JsonDeserializer<Date>)
                  (json, typeOfT, context) -> new Date(json.getAsJsonPrimitive().getAsLong()))
          .create();

  public static RequestBuilder request(
      final String url, final String method, final Map<String, List<String>> queryParams) {
    final RequestBuilder requestBuilder =
        new RequestBuilder()
            .setUrl(url)
            .setMethod(method)
            .addHeader("Accept", "application/json")
            .setRequestTimeout(120000)
            .setReadTimeout(120000);
    if (queryParams != null) {
      requestBuilder.setQueryParams(queryParams);
    }
    return requestBuilder;
  }

  public static org.asynchttpclient.Response execute(
      final CasClient casClient, final Request request) {
    try {
      return casClient.executeBlocking(request);
    } catch (final Exception e) {
      throw new RuntimeException(e);
    }
  }

  public static <T> T get(
      final CasClient casClient,
      final String url,
      final TypeToken<T> typeToken,
      final Map<String, List<String>> queryParams,
      final Integer requestTimeout,
      final Integer readTimeout) {
    final Response response =
        execute(
            casClient,
            request(url, "GET", queryParams)
                .setRequestTimeout(requestTimeout != null ? requestTimeout : 0)
                .setReadTimeout(readTimeout != null ? readTimeout : 0)
                .build());
    if (response.getStatusCode() == 200) {
      return GSON.fromJson(response.getResponseBody(), typeToken.getType());
    } else {
      throw new RuntimeException(
          String.format("Error calling url %s, response code %s", url, response.getStatusCode()));
    }
  }

  public static <T> T post(
      final CasClient casClient,
      final String url,
      final TypeToken<T> typeToken,
      final Object body,
      final Integer requestTimeout,
      final Integer readTimeout) {
    final Response response =
        execute(
            casClient,
            request(url, "POST", null)
                .setBody(GSON.toJson(body))
                .addHeader("Content-Type", "application/json")
                .setRequestTimeout(requestTimeout != null ? requestTimeout : 0)
                .setReadTimeout(readTimeout != null ? readTimeout : 0)
                .build());
    if (response.getStatusCode() == 200) {
      return GSON.fromJson(response.getResponseBody(), typeToken.getType());
    } else {
      throw new RuntimeException(
          String.format("Error calling url %s, response code %s", url, response.getStatusCode()));
    }
  }
}
