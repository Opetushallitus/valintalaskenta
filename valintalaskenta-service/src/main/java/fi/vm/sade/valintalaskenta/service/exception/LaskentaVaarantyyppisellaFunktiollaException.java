package fi.vm.sade.valintalaskenta.service.exception;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Heitetään kun laskentaa yritetään suorittaa totuusarvofunktiolla
 */
public class LaskentaVaarantyyppisellaFunktiollaException extends RuntimeException {

    private static final long serialVersionUID = 1826247254942712989L;

    public LaskentaVaarantyyppisellaFunktiollaException(String message) {
        super(message);
    }

}
