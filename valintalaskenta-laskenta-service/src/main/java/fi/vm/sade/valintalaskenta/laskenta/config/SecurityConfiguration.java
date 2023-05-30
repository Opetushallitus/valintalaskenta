package fi.vm.sade.valintalaskenta.laskenta.config;

import fi.vm.sade.java_utils.security.OpintopolkuCasAuthenticationFilter;
import fi.vm.sade.javautils.kayttooikeusclient.OphUserDetailsServiceImpl;
import fi.vm.sade.properties.OphProperties;
import fi.vm.sade.valintalaskenta.laskenta.config.properties.CasProperties;
import org.jasig.cas.client.session.SingleSignOutFilter;
import org.jasig.cas.client.validation.Cas20ProxyTicketValidator;
import org.jasig.cas.client.validation.TicketValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.security.cas.ServiceProperties;
import org.springframework.security.cas.authentication.CasAuthenticationProvider;
import org.springframework.security.cas.web.CasAuthenticationEntryPoint;
import org.springframework.security.cas.web.CasAuthenticationFilter;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

@Configuration
@EnableMethodSecurity(securedEnabled = true)
@EnableWebSecurity
public class SecurityConfiguration extends WebSecurityConfigurerAdapter {
  private static final Logger LOG = LoggerFactory.getLogger(SecurityConfiguration.class);
  private CasProperties casProperties;
  private OphProperties ophProperties;
  private Environment environment;

  @Autowired
  public SecurityConfiguration(
      final CasProperties casProperties,
      final OphProperties ophProperties,
      final Environment environment) {
    this.casProperties = casProperties;
    this.ophProperties = ophProperties;
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
  public CasAuthenticationProvider casAuthenticationProvider() {
    CasAuthenticationProvider casAuthenticationProvider = new CasAuthenticationProvider();
    String host =
        environment.getProperty(
            "host.host-alb", "https://" + environment.getRequiredProperty("host.host-virkailija"));
    casAuthenticationProvider.setUserDetailsService(
        new OphUserDetailsServiceImpl(host, ConfigEnums.CALLER_ID.value()));
    casAuthenticationProvider.setServiceProperties(serviceProperties());
    casAuthenticationProvider.setTicketValidator(ticketValidator());
    casAuthenticationProvider.setKey(casProperties.getKey());
    return casAuthenticationProvider;
  }

  @Bean
  public TicketValidator ticketValidator() {
    Cas20ProxyTicketValidator ticketValidator =
        new Cas20ProxyTicketValidator(ophProperties.url("cas.url"));
    ticketValidator.setAcceptAnyProxy(true);
    return ticketValidator;
  }

  //
  // CAS filter
  //

  @Bean
  public CasAuthenticationFilter casAuthenticationFilter() throws Exception {
    OpintopolkuCasAuthenticationFilter casAuthenticationFilter =
        new OpintopolkuCasAuthenticationFilter(serviceProperties());
    casAuthenticationFilter.setAuthenticationManager(authenticationManager());
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
    // singleSignOutFilter.setCasServerUrlPrefix(this.ophProperties.url("url-cas"));
    singleSignOutFilter.setIgnoreInitConfiguration(true);
    // TODO konffaa persistoidut sessiot
    // singleSignOutFilter.setSessionMappingStorage(sessionMappingStorage);
    return singleSignOutFilter;
  }

  //
  // CAS entry point
  //

  @Bean
  public CasAuthenticationEntryPoint casAuthenticationEntryPoint() {
    CasAuthenticationEntryPoint casAuthenticationEntryPoint = new CasAuthenticationEntryPoint();
    casAuthenticationEntryPoint.setLoginUrl(ophProperties.url("cas.login"));
    casAuthenticationEntryPoint.setServiceProperties(serviceProperties());
    return casAuthenticationEntryPoint;
  }

  @Override
  protected void configure(HttpSecurity http) throws Exception {
    http.headers()
        .disable()
        .csrf()
        .disable()
        .authorizeRequests()
        .antMatchers("/buildversion.txt")
        .permitAll()
        .antMatchers("/actuator/health")
        .permitAll()
        .antMatchers("/swagger-ui.html")
        .permitAll()
        .antMatchers("/swagger-ui/**")
        .permitAll()
        .antMatchers("/v3/api-docs")
        .permitAll()
        .antMatchers("/v3/api-docs/**")
        .permitAll()
        .anyRequest()
        .authenticated()
        .and()
        .addFilter(casAuthenticationFilter())
        .exceptionHandling()
        .authenticationEntryPoint(casAuthenticationEntryPoint())
        .and()
        .addFilterBefore(singleSignOutFilter(), CasAuthenticationFilter.class);
  }

  @Override
  protected void configure(AuthenticationManagerBuilder auth) throws Exception {
    auth.authenticationProvider(casAuthenticationProvider());
  }
}
