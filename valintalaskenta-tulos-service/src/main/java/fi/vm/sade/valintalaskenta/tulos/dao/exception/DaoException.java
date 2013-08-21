package fi.vm.sade.valintalaskenta.tulos.dao.exception;

/**
 * User: wuoti
 * Date: 19.8.2013
 * Time: 14.18
 */
public class DaoException extends RuntimeException {
    public DaoException() {
    }

    public DaoException(String message) {
        super(message);
    }

    public DaoException(String message, Throwable cause) {
        super(message, cause);
    }

    public DaoException(Throwable cause) {
        super(cause);
    }
}
