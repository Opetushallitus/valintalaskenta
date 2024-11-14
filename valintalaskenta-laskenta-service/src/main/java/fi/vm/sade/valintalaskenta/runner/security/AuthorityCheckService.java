package fi.vm.sade.valintalaskenta.runner.security;

import static java.util.concurrent.TimeUnit.MINUTES;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.runner.resource.external.organisaatio.OrganisaatioAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.TarjontaAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valintalaskenta.runner.util.SecurityUtil;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Service;

@Service
public class AuthorityCheckService {
  private static final Logger LOG = LoggerFactory.getLogger(AuthorityCheckService.class);

  @Autowired private TarjontaAsyncResource tarjontaAsyncResource;
  @Autowired private OrganisaatioAsyncResource organisaatioAsyncResource;
  @Autowired private ValintaperusteetAsyncResource valintaperusteetAsyncResource;

  private HakukohdeOIDAuthorityCheck getAuthorityCheckForRoles(Collection<String> roles) {
    final Collection<String> authorities =
        SecurityUtil.getAuthoritiesFromAuthenticationStartingWith(roles);
    final Set<String> organizationOids =
        SecurityUtil.parseOrganizationOidsFromSecurityRoles(authorities);

    boolean isRootAuthority =
        organizationOids.stream().anyMatch(oid -> SecurityUtil.isRootOrganizationOID(oid));
    if (isRootAuthority) {
      return (oid) -> true;
    } else {
      final Set<String> organizationGroupOids =
          SecurityUtil.parseOrganizationGroupOidsFromSecurityRoles(authorities);
      if (organizationGroupOids.isEmpty() && organizationOids.isEmpty()) {
        throw new RuntimeException("Unauthorized. User has no organization OIDS");
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

      CompletableFuture<HakukohdeOIDAuthorityCheck> authCheck =
          searchByOrganizationOids.thenComposeAsync(
              byOrgs ->
                  searchByOrganizationGroupOids.thenApplyAsync(
                      byGroups -> (oid) -> byOrgs.contains(oid) || byGroups.contains(oid)));
      try {
        return authCheck.get();
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
  }

  public void checkAuthorizationForHaku(String hakuOid, Collection<String> requiredRoles) {
    Collection<? extends GrantedAuthority> userRoles = SecurityUtil.getRoles();

    if (SecurityUtil.containsOphRole(userRoles)) {
      return; // on OPH-käyttäjä, ei tarvitse käydä läpi organisaatioita
    }

    Set<String> tarjoajaOids;
    try {
      tarjoajaOids = tarjontaAsyncResource.haeTarjoajaOids(hakuOid).get(2, MINUTES);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    boolean isAuthorized = isAuthorizedForAnyParentOid(tarjoajaOids, userRoles, requiredRoles);

    if (!isAuthorized) {
      String msg =
          String.format(
              "Käyttäjällä ei oikeutta haun %s tarjoajaan tai sen yläorganisaatioihin.", hakuOid);
      LOG.error(msg);
      throw new AccessDeniedException(msg);
    }
  }

  public void checkAuthorizationForHakukohteet(
      Collection<String> hakukohdeOids, Collection<String> requiredRoles) {
    Collection<? extends GrantedAuthority> userRoles = SecurityUtil.getRoles();

    if (SecurityUtil.containsOphRole(userRoles)) {
      // on OPH-käyttäjä, ei tarvitse käydä läpi organisaatioita
      return;
    }

    HakukohdeOIDAuthorityCheck authCheck = getAuthorityCheckForRoles(requiredRoles);
    Collection<String> notAuthorizedHakukohteet =
        hakukohdeOids.stream().filter(hk -> !authCheck.test(hk)).toList();
    if (notAuthorizedHakukohteet.size() > 0) {
      String msg =
          String.format(
              "Käyttäjällä ei oikeutta seuraaviin hakukohteisiin: %s",
              notAuthorizedHakukohteet.stream().collect(Collectors.joining(",")));
      LOG.error(msg);
      throw new AccessDeniedException(msg);
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
      throw new AccessDeniedException(msg);
    }

    return false;
  }

  public void checkAuthorizationForValintaryhma(
      String valintaryhmaOid, List<String> requiredRoles) {
    Collection<? extends GrantedAuthority> userRoles = SecurityUtil.getRoles();

    boolean isOphUser = SecurityUtil.containsOphRole(userRoles);
    if (isOphUser) {
      return;
    }

    boolean isAuthorized =
        valintaperusteetAsyncResource
            .haeValintaryhmaVastuuorganisaatio(valintaryhmaOid)
            .thenApply(
                vastuuorganisaatioOid -> {
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
            .join();

    if (!isAuthorized) {
      String msg =
          String.format(
              "Käyttäjällä ei oikeutta valintaryhmän %s vastuuorganisaatioon tai sen yläorganisaatioihin.",
              valintaryhmaOid);
      LOG.error(msg);
      throw new AccessDeniedException(msg);
    }
  }

  public void checkAuthorizationForLaskenta(
      LaskentaDto laskentaDto, Collection<String> requiredRoles) {
    if (LaskentaTyyppi.HAKU.equals(laskentaDto.getTyyppi())) {
      this.checkAuthorizationForHaku(laskentaDto.getHakuOid(), requiredRoles);
    } else {
      final List<String> hakukohdeOids =
          laskentaDto.getHakukohteet().stream()
              .map(hk -> hk.getHakukohdeOid())
              .collect(Collectors.toList());
      this.checkAuthorizationForHakukohteet(hakukohdeOids, requiredRoles);
    }
  }
}
