package fi.vm.sade.valinta.kooste.util;

import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.ws.rs.ForbiddenException;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.cas.authentication.CasAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

public class SecurityUtil {
  public static final String ROOTOID = "1.2.246.562.10.00000000001";
  private static final String ORGANIZATION_OID_PREFIX = "1.2.246.562.10";
  private static final String ORGANIZATION_GROUP_OID_PREFIX = "1.2.246.562.28";
  private static final Logger LOG = LoggerFactory.getLogger(SecurityUtil.class);

  public static Collection<String> getAuthoritiesFromAuthenticationStartingWith(
      Collection<String> prefixes) {
    return getAuthoritiesFromAuthentication().stream()
        .filter(auth -> prefixes.stream().anyMatch(prefix -> auth.startsWith(prefix)))
        .collect(Collectors.toSet());
  }

  public static boolean isRootOrganizationOID(String organizationOID) {
    return ROOTOID.equals(organizationOID);
  }

  private static Collection<String> getAuthoritiesFromAuthentication() {
    Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
    if (authentication instanceof CasAuthenticationToken) {
      CasAuthenticationToken casAuthenticationToken = (CasAuthenticationToken) authentication;
      return casAuthenticationToken.getAuthorities().stream()
          .map(a -> a.getAuthority())
          .collect(Collectors.toList());
    } else {
      LOG.error(
          "Tried to get authorities from spring authentication token but token wasn't CAS authentication token");
      return Collections.emptyList();
    }
  }

  static Optional<String> parseOrganizationOidFromSecurityRole(String role) {
    return parseOidFromSecurityRole(role, ORGANIZATION_OID_PREFIX);
  }

  static Optional<String> parseOrganizationGroupOidFromSecurityRole(String role) {
    return parseOidFromSecurityRole(role, ORGANIZATION_GROUP_OID_PREFIX);
  }

  private static Optional<String> parseOidFromSecurityRole(String role, String prefix) {
    String[] pieces = StringUtils.trimToEmpty(role).split("_");
    if (pieces.length > 0) {
      String lastPiece = pieces[pieces.length - 1];
      if (lastPiece.startsWith(prefix)) {
        return Optional.of(lastPiece);
      } else {
        return Optional.empty();
      }
    } else {
      return Optional.empty();
    }
  }

  public static Set<String> parseOrganizationOidsFromSecurityRoles(Collection<String> roles) {
    return roles.stream()
        .flatMap(
            r ->
                parseOrganizationOidFromSecurityRole(r)
                    .map(org -> Stream.of(org))
                    .orElse(Stream.empty()))
        .collect(Collectors.toSet());
  }

  public static Set<String> parseOrganizationGroupOidsFromSecurityRoles(Collection<String> roles) {
    return roles.stream()
        .flatMap(
            r ->
                parseOrganizationGroupOidFromSecurityRole(r)
                    .map(org -> Stream.of(org))
                    .orElse(Stream.empty()))
        .collect(Collectors.toSet());
  }

  public static Collection<? extends GrantedAuthority> getRoles() {
    SecurityContext context = SecurityContextHolder.getContext();
    if (context == null) {
      String msg = "No SecurityContext found";
      throw new ForbiddenException(msg);
    }

    Authentication authentication = context.getAuthentication();
    if (authentication == null) {
      String msg = "No Authentication found in SecurityContext";
      throw new ForbiddenException(msg);
    }

    return authentication.getAuthorities();
  }

  public static boolean containsOphRole(Collection<? extends GrantedAuthority> userRoles) {
    for (GrantedAuthority auth : userRoles) {
      Optional<String> optionalOID = parseOrganizationOidFromSecurityRole(auth.getAuthority());
      if (optionalOID.isPresent() && isRootOrganizationOID(optionalOID.get())) return true;
    }

    return false;
  }
}
