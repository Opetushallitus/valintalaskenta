package fi.vm.sade.service.valintalaskenta.service.impl.conversion;

import fi.vm.sade.service.valintalaskenta.service.exception.ToteutusEiVastaaPalveluRajapintaaException;
import fi.vm.sade.service.valintaperusteet.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.schema.FunktiokutsunimiTyyppi;
import org.springframework.core.convert.converter.Converter;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public class FunktionimiTyyppiToFunktionimiConverter implements Converter<FunktiokutsunimiTyyppi, Funktionimi> {

    public Funktionimi convert(FunktiokutsunimiTyyppi source) {
        assert (Funktionimi.values().length == FunktiokutsunimiTyyppi.values().length);
        switch (source) {
        case EI:
            return Funktionimi.EI;
        case HAELUKUARVO:
            return Funktionimi.HAELUKUARVO;
        case HAEMERKKIJONOJAKONVERTOILUKUARVOKSI:
            return Funktionimi.HAEMERKKIJONOJAKONVERTOILUKUARVOKSI;
        case HAETOTUUSARVO:
            return Funktionimi.HAETOTUUSARVO;
        case JA:
            return Funktionimi.JA;
        case JOS:
            return Funktionimi.JOS;
        case KESKIARVO:
            return Funktionimi.KESKIARVO;
        case KESKIARVONPARASTA:
            return Funktionimi.KESKIARVONPARASTA;
        case LUKUARVO:
            return Funktionimi.LUKUARVO;
        case KONVERTOILUKUARVO:
            return Funktionimi.KONVERTOILUKUARVO;
        case MAKSIMI:
            return Funktionimi.MAKSIMI;
        case MEDIAANI:
            return Funktionimi.MEDIAANI;
        case MINIMI:
            return Funktionimi.MINIMI;
        case NEGAATIO:
            return Funktionimi.NEGAATIO;
        case NIMETTYLUKUARVO:
            return Funktionimi.NIMETTYLUKUARVO;
        case NIMETTYTOTUUSARVO:
            return Funktionimi.NIMETTYTOTUUSARVO;
        case NMAKSIMI:
            return Funktionimi.NMAKSIMI;
        case NMINIMI:
            return Funktionimi.NMINIMI;
        case OSAMAARA:
            return Funktionimi.OSAMAARA;
        case PIENEMPI:
            return Funktionimi.PIENEMPI;
        case PIENEMPITAIYHTASUURI:
            return Funktionimi.PIENEMPITAIYHTASUURI;
        case SUMMA:
            return Funktionimi.SUMMA;
        case SUMMANPARASTA:
            return Funktionimi.SUMMANPARASTA;
        case SUUREMPI:
            return Funktionimi.SUUREMPI;
        case SUUREMPITAIYHTASUURI:
            return Funktionimi.SUUREMPITAIYHTASUURI;
        case TAI:
            return Funktionimi.TAI;
        case TOTUUSARVO:
            return Funktionimi.TOTUUSARVO;
        case TULO:
            return Funktionimi.TULO;
        case TYHJA:
            return Funktionimi.TYHJA;
        case YHTASUURI:
            return Funktionimi.YHTASUURI;
        case HAKUTOIVE:
            return Funktionimi.HAKUTOIVE;

        default:
            throw new ToteutusEiVastaaPalveluRajapintaaException(
                    "Switch-lauseke ei ole ajantasalla! Lausekkeesta puuttuu " + source + "-tyyppi!");
        }
    }

}
