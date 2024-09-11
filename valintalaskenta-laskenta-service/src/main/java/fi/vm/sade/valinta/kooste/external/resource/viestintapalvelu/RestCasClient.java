package fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.javautils.nio.cas.CasClient;
import fi.vm.sade.javautils.nio.cas.CasClientBuilder;
import fi.vm.sade.javautils.nio.cas.CasConfig;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import org.asynchttpclient.Request;
import org.asynchttpclient.RequestBuilder;
import org.asynchttpclient.Response;

/**
 * Wrapper-luokka CasClietille. Tätä käytetään jotta Spring Boot -migraation yhteydessä testien
 * mock-lähestymistapaa ei jouduttu refaktoroimaan isosti.
 */
public class RestCasClient {

  private static final String CALLER_ID = "1.2.246.562.10.00000000001.valintalaskentakoostepalvelu";
  private static final String CSRF_VALUE = "CSRF";

  private Function<Request, CompletableFuture<Response>> executor;

  public RestCasClient(CasConfig casConfig) {
    this(casConfig, DateDeserializer.gsonBuilder().create());
  }

  public RestCasClient(CasConfig casConfig, Gson gson) {
    CasClient casClient = CasClientBuilder.build(casConfig);
    this.executor =
        request ->
            casClient.executeAndRetryWithCleanSessionOnStatusCodes(request, Set.of(302, 401));
    this.gson = gson;
  }

  protected RestCasClient(Function<Request, CompletableFuture<Response>> executor) {
    this.executor = executor;
    this.gson = DateDeserializer.gsonBuilder().create();
  }

  protected Gson gson;

  public <T> CompletableFuture<T> post(
      final String url,
      final TypeToken<T> typeToken,
      final Object body,
      final Map<String, String> headers,
      final int requestTimeout) {
    return this.post(url, body, headers, requestTimeout)
        .thenApply(response -> this.gson.fromJson(response.getResponseBody(), typeToken.getType()));
  }

  public <T> CompletableFuture<T> postPlaintext(
      final String url,
      final TypeToken<T> typeToken,
      final Object body,
      final Map<String, String> headers,
      final int requestTimeout) {
    return this.postPlaintext(url, body, headers, requestTimeout)
        .thenApply(response -> this.gson.fromJson(response.getResponseBody(), typeToken.getType()));
  }

  public CompletableFuture<Response> post(
      final String url, final Object body, final Map<String, String> headers, final int timeout) {
    return this.executeAndThrowOnError(
        withHeaders(request(url, "POST", timeout).setBody(this.gson.toJson(body)), headers)
            .build());
  }

  public CompletableFuture<Response> postPlaintext(
      final String url, final Object body, final Map<String, String> headers, final int timeout) {
    return this.executeAndThrowOnError(
        withHeaders(request(url, "POST", timeout).setBody(body.toString()), headers).build());
  }

  public <T> CompletableFuture<T> get(
      final String url,
      final TypeToken<T> typeToken,
      final Map<String, String> headers,
      final int timeout) {
    return this.get(url, headers, timeout)
        .thenApply(response -> this.gson.fromJson(response.getResponseBody(), typeToken.getType()));
  }

  public CompletableFuture<Response> get(
      final String url, final Map<String, String> headers, final int timeout) {
    return this.executeAndThrowOnError(withHeaders(request(url, "GET", timeout), headers).build());
  }

  public <T> CompletableFuture<T> put(
      final String url,
      final TypeToken<T> typeToken,
      final Object body,
      final Map<String, String> headers,
      final int timeout) {
    return this.put(url, body, headers, timeout)
        .thenApply(response -> this.gson.fromJson(response.getResponseBody(), typeToken.getType()));
  }

  public CompletableFuture<Response> put(
      final String url, final Object body, final Map<String, String> headers, final int timeout) {
    return this.executeAndThrowOnError(
        withHeaders(request(url, "PUT", timeout).setBody(this.gson.toJson(body)), headers).build());
  }

  public CompletableFuture<String> delete(
      final String url, final Map<String, String> headers, final int timeout) {
    return this.executeAndThrowOnError(
            withHeaders(request(url, "DELETE", timeout), headers).build())
        .thenApply(r -> r.getResponseBody());
  }

  public static class RestCasClientException extends RuntimeException {

    private Response response;

    public RestCasClientException(Response response, String message) {
      super(message);
      this.response = response;
    }

    public Response getResponse() {
      return this.response;
    }
  }

  private CompletableFuture<Response> executeAndThrowOnError(Request request) {
    return this.executor
        .apply(request)
        .thenApply(
            response -> {
              if (response.getStatusCode() == 200
                  || response.getStatusCode() == 202
                  || response.getStatusCode() == 204) {
                return response;
              } else {
                throw new RestCasClientException(
                    response,
                    String.format(
                        "Error calling url %s with method %s, response code %s",
                        request.getUrl(), request.getMethod(), response.getStatusCode()));
              }
            });
  }

  private static RequestBuilder request(final String url, final String method, int timeout) {
    final RequestBuilder requestBuilder =
        new RequestBuilder()
            .setUrl(url)
            .setMethod(method)
            .setRequestTimeout(timeout)
            .setReadTimeout(timeout)
            .addHeader("Caller-Id", CALLER_ID)
            .addHeader("CSRF", CSRF_VALUE)
            .addHeader("Cookie", String.format("CSRF=%s;", CSRF_VALUE));
    return requestBuilder;
  }

  private static RequestBuilder withHeaders(RequestBuilder builder, Map<String, String> headers) {
    builder = builder.build().toBuilder();
    for (Map.Entry<String, String> header : headers.entrySet()) {
      builder.setHeader(header.getKey(), header.getValue());
    }
    if (!headers.containsKey("Content-Type")) builder.addHeader("Content-Type", "application/json");
    if (!headers.containsKey("Accept")) builder.addHeader("Accept", "application/json");
    return builder;
  }
}
