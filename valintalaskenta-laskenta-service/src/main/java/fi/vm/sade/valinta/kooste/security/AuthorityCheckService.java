package fi.vm.sade.valinta.kooste.security;

import static fi.vm.sade.valinta.kooste.util.SecurityUtil.*;
import static java.util.concurrent.TimeUnit.MINUTES;

import fi.vm.sade.valinta.kooste.external.resource.organisaatio.OrganisaatioAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.TarjontaAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import io.reactivex.Observable;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import javax.ws.rs.ForbiddenException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

@Service
public class AuthorityCheckService {
  private static final Logger LOG = LoggerFactory.getLogger(AuthorityCheckService.class);

  @Autowired private TarjontaAsyncResource tarjontaAsyncResource;
  @Autowired private OrganisaatioAsyncResource organisaatioAsyncResource;
  @Autowired private ValintaperusteetAsyncResource valintaperusteetAsyncResource;

  public CompletableFuture<HakukohdeOIDAuthorityCheck> getAuthorityCheckForRoles(
      Collection<String> roles) {
    final Collection<String> authorities = getAuthoritiesFromAuthenticationStartingWith(roles);
    final Set<String> organizationOids = parseOrganizationOidsFromSecurityRoles(authorities);
    boolean isRootAuthority = organizationOids.stream().anyMatch(oid -> isRootOrganizationOID(oid));
    if (isRootAuthority) {
      return CompletableFuture.completedFuture((oid) -> true);
    } else {
      final Set<String> organizationGroupOids =
          parseOrganizationGroupOidsFromSecurityRoles(authorities);
      if (organizationGroupOids.isEmpty() && organizationOids.isEmpty()) {
        return CompletableFuture.failedFuture(
            new RuntimeException("Unauthorized. User has no organization OIDS"));
      }
      CompletableFuture<Set<String>> searchByOrganizationOids =
          Optional.of(organizationOids)
              .filter(oids -> !oids.isEmpty())
              .map(tarjontaAsyncResource::hakukohdeSearchByOrganizationOids)
              .orElse(CompletableFuture.completedFuture(Collections.emptySet()));

      CompletableFuture<Set<String>> searchByOrganizationGroupOids =
          Optional.of(organizationGroupOids)
              .filter(oids -> !oids.isEmpty())
              .map(tarjontaAsyncResource::hakukohdeSearchByOrganizationGroupOids)
              .orElse(CompletableFuture.completedFuture(Collections.emptySet()));

      return searchByOrganizationOids.thenComposeAsync(
          byOrgs ->
              searchByOrganizationGroupOids.thenApplyAsync(
                  byGroups -> (oid) -> byOrgs.contains(oid) || byGroups.contains(oid)));
    }
  }

  public void checkAuthorizationForHaku(String hakuOid, Collection<String> requiredRoles) {
    Collection<? extends GrantedAuthority> userRoles = getRoles();

    if (containsOphRole(userRoles)) {
      // on OPH-käyttäjä, ei tarvitse käydä läpi organisaatioita
      return;
    }

    boolean isAuthorized =
        Observable.fromFuture(tarjontaAsyncResource.haeHaku(hakuOid))
            .map(haku -> isAuthorizedForAnyParentOid(haku.tarjoajaOids, userRoles, requiredRoles))
            .timeout(2, MINUTES)
            .blockingFirst();

    if (!isAuthorized) {
      String msg =
          String.format(
              "Käyttäjällä ei oikeutta haun %s tarjoajaan tai sen yläorganisaatioihin.", hakuOid);
      LOG.error(msg);
      throw new ForbiddenException(msg);
    }
  }

  public void checkAuthorizationForHakukohteet(
      Collection<String> hakukohdeOids, Collection<String> requiredRoles) {
    Collection<? extends GrantedAuthority> userRoles = getRoles();

    if (containsOphRole(userRoles)) {
      // on OPH-käyttäjä, ei tarvitse käydä läpi organisaatioita
      return;
    }

    boolean isAuthorized =
        Observable.fromFuture(getAuthorityCheckForRoles(requiredRoles))
            .map(authorityCheck -> hakukohdeOids.stream().anyMatch(authorityCheck))
            .timeout(2, MINUTES)
            .blockingFirst();

    if (!isAuthorized) {
      String msg =
          String.format("Käyttäjällä ei oikeutta yhteenkään hakukohteeseen: %s", hakukohdeOids);
      LOG.error(msg);
      throw new ForbiddenException(msg);
    }
  }

  public HakukohdeOIDAuthorityCheck getHakukohdeOidBasedAuthCheck(
      Context context, Collection<String> requiredRoles) {
    setContext(context);
    Collection<? extends GrantedAuthority> userRoles = getRoles();
    if (containsOphRole(userRoles)) {
      return (oid) -> true;
    } else {
      HakukohdeOIDAuthorityCheck authCheck =
          Observable.fromFuture(getAuthorityCheckForRoles(requiredRoles))
              .timeout(2, MINUTES)
              .blockingFirst();
      clearContext();
      return authCheck;
    }
  }

  public boolean isAuthorizedForAnyParentOid(
      Set<String> organisaatioOids,
      Collection<? extends GrantedAuthority> userRoles,
      Collection<String> requiredRoles) {
    try {
      for (String organisaatioOid : organisaatioOids) {
        String parentOidsPath = organisaatioAsyncResource.parentoids(organisaatioOid).get();
        String[] parentOids = parentOidsPath.split("/");

        for (String oid : parentOids) {
          for (String role : requiredRoles) {
            String organizationRole = role + "_" + oid;

            for (GrantedAuthority auth : userRoles) {
              if (organizationRole.equals(auth.getAuthority())) return true;
            }
          }
        }
      }
    } catch (Exception e) {
      String msg =
          String.format(
              "Organisaatioiden %s parentOids -haku epäonnistui",
              String.join(", ", organisaatioOids));
      LOG.error(msg, e);
      throw new ForbiddenException(msg);
    }

    return false;
  }

  public void checkAuthorizationForValintaryhma(
      String valintaryhmaOid, List<String> requiredRoles) {
    Collection<? extends GrantedAuthority> userRoles = getRoles();

    boolean isOphUser = containsOphRole(userRoles);
    if (isOphUser) {
      return;
    }

    boolean isAuthorized =
        valintaperusteetAsyncResource
            .haeValintaryhmaVastuuorganisaatio(valintaryhmaOid)
            .map(
                (vastuuorganisaatioOid) -> {
                  if (vastuuorganisaatioOid == null) {
                    LOG.error(
                        "Valintaryhmän {} vastuuorganisaatio on null; vain OPH:lla oikeus valintaryhmään.",
                        valintaryhmaOid);
                    return false;
                  } else {
                    return isAuthorizedForAnyParentOid(
                        Collections.singleton(vastuuorganisaatioOid), userRoles, requiredRoles);
                  }
                })
            .timeout(2, MINUTES)
            .blockingFirst();

    if (!isAuthorized) {
      String msg =
          String.format(
              "Käyttäjällä ei oikeutta valintaryhmän %s vastuuorganisaatioon tai sen yläorganisaatioihin.",
              valintaryhmaOid);
      LOG.error(msg);
      throw new ForbiddenException(msg);
    }
  }

  /**
   * Käyttöoikeustarkastelun konteksti
   *
   * <p>Tämän avulla käyttöoikeustarkastelun voi siirtää käyttäjän tunnistaneesta säikeestä toiseen
   * säikeeseen.
   */
  public static class Context {
    protected final SecurityContext securityContext;

    protected Context(SecurityContext securityContext) {
      this.securityContext = securityContext;
    }

    protected SecurityContext getSecurityContext() {
      return securityContext;
    }
  }

  public Context getContext() {
    return new Context(SecurityContextHolder.getContext());
  }

  public void withContext(Context context, Runnable callback) {
    setContext(context);
    callback.run();
    clearContext();
  }

  private void setContext(Context context) {
    SecurityContextHolder.setContext(context.getSecurityContext());
  }

  private void clearContext() {
    SecurityContextHolder.clearContext();
  }
}
