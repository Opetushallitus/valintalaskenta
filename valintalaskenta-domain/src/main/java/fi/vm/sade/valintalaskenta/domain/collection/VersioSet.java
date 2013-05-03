package fi.vm.sade.valintalaskenta.domain.collection;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Tarjoaa rajapinnan josta näkee versioiden järjestyslogiikan. Ei
 *         toteutua uutta toiminnallisuutta TreeSettiin.
 */
public class VersioSet<E> extends TreeSet<E> {

    public VersioSet() {
        super();
    }

    public VersioSet(SortedSet<E> s) {
        super(s);
    }

    public VersioSet(Collection<E> c) {
        super(c);
    }

    public E haeUusinVersio() {
        return last();
    }

    public E haeVanhinVersio() {
        return first();
    }

    private static final long serialVersionUID = -8220821651147875521L;

}
