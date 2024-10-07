package fi.vm.sade.valintalaskenta.laskenta.config;

import fi.vm.sade.java_utils.security.OpintopolkuCasAuthenticationFilter;
import fi.vm.sade.javautils.kayttooikeusclient.OphUserDetailsServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.config.properties.CasProperties;
import org.apereo.cas.client.session.SingleSignOutFilter;
import org.apereo.cas.client.validation.Cas20ProxyTicketValidator;
import org.apereo.cas.client.validation.TicketValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.cas.ServiceProperties;
import org.springframework.security.cas.authentication.CasAuthenticationProvider;
import org.springframework.security.cas.web.CasAuthenticationEntryPoint;
import org.springframework.security.cas.web.CasAuthenticationFilter;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.session.jdbc.config.annotation.web.http.EnableJdbcHttpSession;
import org.springframework.session.web.http.CookieSerializer;
import org.springframework.session.web.http.DefaultCookieSerializer;

@Profile({"default", "dev"})
@Configuration
@EnableMethodSecurity(securedEnabled = true)
@EnableWebSecurity
@EnableJdbcHttpSession
public class SecurityConfiguration {
  private static final Logger LOG = LoggerFactory.getLogger(SecurityConfiguration.class);
  private CasProperties casProperties;
  private Environment environment;

  @Autowired
  public SecurityConfiguration(final CasProperties casProperties, final Environment environment) {
    this.casProperties = casProperties;
    this.environment = environment;
    LOG.info("CAS props: " + casProperties);
  }

  @Bean
  public ServiceProperties serviceProperties() {
    ServiceProperties serviceProperties = new ServiceProperties();
    serviceProperties.setService(casProperties.getService() + "/j_spring_cas_security_check");
    serviceProperties.setSendRenew(casProperties.getSendRenew());
    serviceProperties.setAuthenticateAllArtifacts(true);
    return serviceProperties;
  }

  //
  // CAS authentication provider (authentication manager)
  //

  @Bean
  public CasAuthenticationProvider casAuthenticationProvider(TicketValidator ticketValidator) {
    CasAuthenticationProvider casAuthenticationProvider = new CasAuthenticationProvider();
    casAuthenticationProvider.setAuthenticationUserDetailsService(new OphUserDetailsServiceImpl());
    casAuthenticationProvider.setServiceProperties(serviceProperties());
    casAuthenticationProvider.setTicketValidator(ticketValidator);
    casAuthenticationProvider.setKey(casProperties.getKey());
    return casAuthenticationProvider;
  }

  @Bean
  public TicketValidator ticketValidator() {
    Cas20ProxyTicketValidator ticketValidator =
        new Cas20ProxyTicketValidator(environment.getRequiredProperty("cas.url"));
    ticketValidator.setAcceptAnyProxy(true);
    return ticketValidator;
  }

  //
  // CAS filter
  //

  @Bean
  public CasAuthenticationFilter casAuthenticationFilter(
      ServiceProperties serviceProperties, AuthenticationManager authenticationManager)
      throws Exception {
    OpintopolkuCasAuthenticationFilter casAuthenticationFilter =
        new OpintopolkuCasAuthenticationFilter(serviceProperties);
    casAuthenticationFilter.setAuthenticationManager(authenticationManager);
    casAuthenticationFilter.setFilterProcessesUrl("/j_spring_cas_security_check");
    return casAuthenticationFilter;
  }

  //
  // CAS single logout filter
  // requestSingleLogoutFilter is not configured because our users always sign out through CAS
  // logout (using virkailija-raamit
  // logout button) when CAS calls this filter if user has ticket to this service.
  //
  @Bean
  public SingleSignOutFilter singleSignOutFilter() {
    SingleSignOutFilter singleSignOutFilter = new SingleSignOutFilter();
    singleSignOutFilter.setIgnoreInitConfiguration(true);
    return singleSignOutFilter;
  }

  //
  // CAS entry point
  //

  @Bean
  public CasAuthenticationEntryPoint casAuthenticationEntryPoint(
      ServiceProperties serviceProperties) {
    CasAuthenticationEntryPoint casAuthenticationEntryPoint = new CasAuthenticationEntryPoint();
    casAuthenticationEntryPoint.setLoginUrl(environment.getRequiredProperty("cas.login"));
    casAuthenticationEntryPoint.setServiceProperties(serviceProperties);
    return casAuthenticationEntryPoint;
  }

  @Bean
  public SecurityFilterChain configureFilterChain(
      HttpSecurity http,
      CasAuthenticationFilter casAuthenticationFilter,
      SingleSignOutFilter singleSignOutFilter,
      CasAuthenticationEntryPoint casAuthenticationEntryPoint)
      throws Exception {
    return http.headers(h -> h.disable())
        .csrf(c -> c.disable())
        .authorizeHttpRequests(
            requests ->
                requests
                    .requestMatchers(
                        HttpMethod.GET,
                        "/buildversion.txt",
                        "/actuator/health",
                        "/v3/api-docs",
                        "/v3/api-docs/**",
                        "/swagger",
                        "/swagger/**",
                        "/swagger-ui/**",
                        "/swagger-ui.html",
                        "/webjars/swagger-ui/**")
                    .permitAll()
                    .anyRequest()
                    .fullyAuthenticated())
        .exceptionHandling(e -> e.authenticationEntryPoint(casAuthenticationEntryPoint))
        .addFilter(casAuthenticationFilter)
        .addFilterBefore(singleSignOutFilter, CasAuthenticationFilter.class)
        .build();
  }

  @Bean
  protected AuthenticationManager configure(
      HttpSecurity http, CasAuthenticationProvider casAuthenticationProvider) throws Exception {
    return http.getSharedObject(AuthenticationManagerBuilder.class)
        .authenticationProvider(casAuthenticationProvider)
        .build();
  }

  @Bean
  public CookieSerializer cookieSerializer() {
    DefaultCookieSerializer serializer = new DefaultCookieSerializer();
    serializer.setUseSecureCookie(true);
    serializer.setCookieName("JSESSIONID");
    serializer.setCookiePath("/valintalaskenta-laskenta-service");
    // tämä jotta yliheitto toimii, ks. https://github.com/spring-projects/spring-session/issues/1201
    serializer.setUseBase64Encoding(false);
    return serializer;
  }
}
