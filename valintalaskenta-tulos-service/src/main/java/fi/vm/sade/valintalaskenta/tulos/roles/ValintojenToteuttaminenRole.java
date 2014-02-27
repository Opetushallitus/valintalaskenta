package fi.vm.sade.valintalaskenta.tulos.roles;

/**
 * Created with IntelliJ IDEA.
 * User: jukais
 * Date: 3.4.2013
 * Time: 15.12
 * To change this template use File | Settings | File Templates.
 */
public class ValintojenToteuttaminenRole {
    public static final String READ_UPDATE_CRUD = "hasAnyRole('ROLE_APP_VALINTOJENTOTEUTTAMINEN_READ','ROLE_APP_VALINTOJENTOTEUTTAMINEN_READ_UPDATE','ROLE_APP_VALINTOJENTOTEUTTAMINEN_CRUD')";
    public static final String UPDATE_CRUD = "hasAnyRole('ROLE_APP_VALINTOJENTOTEUTTAMINEN_READ_UPDATE','ROLE_APP_VALINTOJENTOTEUTTAMINEN_CRUD')";
    public static final String CRUD = "hasAnyRole('ROLE_APP_VALINTOJENTOTEUTTAMINEN_CRUD')";
    public static final String OPH_CRUD = "hasAnyRole('ROLE_APP_VALINTOJENTOTEUTTAMINEN_CRUD_1.2.246.562.10.00000000001')";
}
