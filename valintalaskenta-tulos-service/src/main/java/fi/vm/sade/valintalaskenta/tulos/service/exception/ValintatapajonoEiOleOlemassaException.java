package fi.vm.sade.valintalaskenta.tulos.service.exception;

/**
 * User: wuoti
 * Date: 23.5.2013
 * Time: 14.36
 */
public class ValintatapajonoEiOleOlemassaException extends RuntimeException {
    public ValintatapajonoEiOleOlemassaException() {
    }

    public ValintatapajonoEiOleOlemassaException(String message) {
        super(message);
    }

    public ValintatapajonoEiOleOlemassaException(String message, Throwable cause) {
        super(message, cause);
    }

    public ValintatapajonoEiOleOlemassaException(Throwable cause) {
        super(cause);
    }
}
