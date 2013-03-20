package fi.vm.sade.valintalaskenta.laskenta.service.exception;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Heitetään jos WSDL-rajapinta tarjoaa funktiotyyppejä joita
 *         funktiokonvertteri ei tunnista
 */
public class ToteutusEiVastaaPalveluRajapintaaException extends RuntimeException {

    private static final long serialVersionUID = -7079716417505118169L;

    public ToteutusEiVastaaPalveluRajapintaaException(String message) {
        super(message);
    }
}
