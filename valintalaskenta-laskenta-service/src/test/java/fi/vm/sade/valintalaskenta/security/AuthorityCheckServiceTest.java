package fi.vm.sade.valintalaskenta.security;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import fi.vm.sade.valintalaskenta.runner.resource.external.organisaatio.OrganisaatioAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.TarjontaAsyncResource;
import fi.vm.sade.valintalaskenta.runner.security.AuthorityCheckService;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.GrantedAuthority;

public class AuthorityCheckServiceTest {

  @InjectMocks private AuthorityCheckService authorityCheckService;

  @Mock private OrganisaatioAsyncResource organisaatioAsyncResource;

  @Mock private TarjontaAsyncResource tarjontaAsyncResource;

  @BeforeEach
  public void initMocks() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.when(organisaatioAsyncResource.parentoids("oid.1"))
        .thenReturn(CompletableFuture.completedFuture("parent.oid.1/oid.1"));
    Mockito.when(organisaatioAsyncResource.parentoids("oid.2"))
        .thenReturn(CompletableFuture.completedFuture("parent.oid.2/oid.2"));
    Mockito.when(organisaatioAsyncResource.parentoids("oid.3"))
        .thenReturn(CompletableFuture.completedFuture("parent.oid.3/oid.3"));
    Mockito.when(tarjontaAsyncResource.haeTarjoajaOids("haku.oid"))
        .thenReturn(CompletableFuture.completedFuture(new HashSet<>()));
  }

  private class TestAuthority implements GrantedAuthority {

    public TestAuthority(String role) {
      this.role = role;
    }

    private String role;

    public String getAuthority() {
      return role;
    }
  }

  @Test
  public void testIsAuthorizedForAnyParentOid() {
    Set<String> organisaatioOids = new HashSet<>();
    organisaatioOids.add("oid.1");
    organisaatioOids.add("oid.2");
    organisaatioOids.add("oid.3");
    Collection<? extends GrantedAuthority> userRoles =
        Collections.singleton(new TestAuthority("authorized_parent.oid.3"));
    Collection<String> requiredRoles = Collections.singleton("authorized");

    boolean authorized =
        authorityCheckService.isAuthorizedForAnyParentOid(
            organisaatioOids, userRoles, requiredRoles);
    assertTrue(authorized);

    Set<String> organisaatioOids2 = new HashSet<>();
    organisaatioOids2.add("oid.1");
    organisaatioOids2.add("oid.2");

    boolean authorized2 =
        authorityCheckService.isAuthorizedForAnyParentOid(
            organisaatioOids2, userRoles, requiredRoles);
    assertFalse(authorized2);
  }

  @Disabled("Causes possibly random failure in CI")
  @Test
  public void testCheckAuthorizationForHaku() {
    Assertions.assertThrows(
        AccessDeniedException.class,
        () -> authorityCheckService.checkAuthorizationForHaku("haku.oid", Collections.EMPTY_SET));
  }
}
