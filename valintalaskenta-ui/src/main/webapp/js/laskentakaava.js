
var Kaava = function(dsl, data) {
    this.dsl = dsl
    this.data = data
    this.funktio = new Funktio(this.dsl, this.data.funktiokutsu)
    this.funktio.init()

    /* Structure methods i.e. parses subitems */
    this.funktiopuu = function() {
        return this.funktio
    }

    this.luonnos = function() {
        return this.data.onLuonnos
    }

    /* UI Methods */
    this.getAllErrors = function() {
        return this.funktio.getAllErrors()
    }

    this.hasErrors = function() {
        return this.getAllErrors().length > 0
    }

    this.getData = function() {
        var removeByAttr = function(arr, attr){
            for(var i in arr) {
                if(arr[i] && arr[i][attr]){
                    delete arr[i][attr]
                } else if(typeof arr[i] === 'object' || typeof arr[i] === 'array') {
                    removeByAttr(arr[i], attr)
                }
            }
            return arr;
        }
        var removeMarkedFunktioargumentit = function(o) {
            for(var i in o) {
                if(o[i] && o[i].child && o[i].child.deleted) {
                    o.splice(i, 1)
                } else if(typeof o[i] === 'object' || typeof o[i] === 'array') {
                    removeMarkedFunktioargumentit(o[i])
                }
            }
            return o
        }
        return removeMarkedFunktioargumentit(removeByAttr(this.data, 'validointivirheet'))
    }

}

var TyhjaFunktio = function(def) {
    this.template = function() {
        return "tyhja_template.html"
    }

    this.nimi = function() {
        return def.nimi
    }

    this.otsikko = function() {
        return def.otsikko || def.nimi
    }

    this.errors = function() {
        return []
    }

    this.isLukuarvoFunktio = function() {
        return def.tyyppi == "LUKUARVOFUNKTIO"
    }

    this.isTotuusarvoFunktio = function() {
        return def.tyyppi == "TOTUUSARVOFUNKTIO"
    }
}

var Funktio = function(dsl, data) {

    var HAETTAVA_TYYPPI = ["HAELUKUARVO"]
    var NIMETTAVAT_TYYPPI = ["NIMETTYLUKUARVO", "NIMETTYTOTUUSARVO"]

    this.dsl = dsl
    this.data = data


    /* Structure methods i.e. parses subitems */
    this.getId = function() {
        return this.data.id
    }

    this.getNimi = function() {
        return this.data.funktionimi
    }
    this.tyyppi = function() {
        return this.findFunctionDefinitionByFunktionimi(this.nimi)
    }

    /**
     * Palauttaa true, jos funktiolla on nimettyjä funktioargumentteja. Esim. osamäärä.
     * @return {boolean}
     */
    this.hasNimetytArgumentit = function() {
        // Tarkista, onko n kardinaliteetti
        var fdef = this.findFunctionDefinitionByFunktionimi(this.nimi);
        if(!fdef.funktioargumentit) {
            return false
        }
        if(fdef.funktioargumentit.length == 1
            && fdef.funktioargumentit[0].kardinaliteetti == "n") {
            return false
        } else {
            return true
        }
    }

    /**
     * Luo funktioargumenttioliot.
     * @return {Array}
     */
    this.getFunktioargumentit = function() {
        if(this.hasNimetytArgumentit()) {
            return this.getFArgsNimetty()
        } else {
            return this.getFunktioargumentitN();
        }
    }

    /**
     * Luo funktioargumenttioliot, jos nimettyjä alifunktioita.
     * @return {Array}
     */
    this.getFArgsNimetty = function () {
        var funcArgs = []
        var fdef = this.findFunctionDefinitionByFunktionimi(this.nimi);
        var argCount = fdef.funktioargumentit.length
        for(var i = 0; i < argCount; i++) {
            var arg = this.data.funktioargumentit.filter(function(arg) {
                // Kallen indeksit alkaa ykkösestä.
                return arg.indeksi == i + 1
            })
            if(arg.length == 0) {
                funcArgs[i] = new TyhjaFunktio(fdef.funktioargumentit[i]);
            } else if (arg.length == 1) {
                var f = new Funktio(this.dsl, arg[0].child)
                f.init()
                funcArgs[i] = f
            } else {
                throw new Exception("Found too many objects from indeksi " + (i + 1))
            }
        }
        return funcArgs
    }

    /**
     * Luo funktioargumenttioliot, jos ei ole nimettyjä funktioargumentteja.
     * @return {Array}
     */
    this.getFunktioargumentitN = function() {
        var fdef = this.findFunctionDefinitionByFunktionimi(this.nimi);
        if(!fdef || !fdef.funktioargumentit || fdef.funktioargumentit.length < 1) {
            return
        }
        var funcargs = []
        for(i in this.data.funktioargumentit) {
            var arg = this.data.funktioargumentit[i].child
            var f = new Funktio(this.dsl, arg)
            f.init()
            funcargs.push(f)
        }
        funcargs.push(new TyhjaFunktio({
                otsikko: 'Lisää uusi funktio',
                nimi: null,
                tyyppi: fdef.funktioargumentit[0].tyyppi
            })
        )
        return funcargs
    }

    /**
     * Luo Parametri-oliot kaikille syötettäville parametreille.
     * @return {Array}
     */
    this.getSyoteparametrit = function() {
        var funcdef = this.findFunctionDefinitionByFunktionimi(this.nimi);
        var params = []
        for(i in funcdef.syoteparametrit) {
            var paramDef = funcdef.syoteparametrit[i];
            var paramData = this.data.syoteparametrit.filter(function(param) {
                return param.avain = paramDef.avain
            })[0]
            params.push(new Parametri(paramDef, paramData))
        }
        return params
    }

    /* Helper methods */

    /**
     * Hakee funktion määrittelyt DSL:stä nimen perusteella.
     * @param {String} nimi
     * @return {object}
     */
    this.findFunctionDefinitionByFunktionimi = function(nimi) {
        return this.findFunctionDefinition(function(fdef) {
            return nimi == fdef.nimi
        })
    }

    /**
     * Filtteröi funktio DSL:stä funktiokuvauksia annetun funktion perusteella.
     * @param func
     * @return {object}
     */
    this.findFunctionDefinition = function(func) {
        var def = this.dsl.filter(func)
        if(def) {
            return def[0]
        }
        return null
    }

    this.init = function() {
        this.nimi = this.getNimi()
        this.funktioargumentit = this.getFunktioargumentit();

        // Used for comparison.
        this.hashKey = Math.random().toString(36).substring(7);
        this.syoteparametrit = this.getSyoteparametrit()
        //this.konverteriparametrit = this.getKonverteriParametrit()
        this.konverteri = this.getKonverteri()
    }

    /* UI methods */
    this.template = function() {
        var labelFunctions = ["NIMETTYLUKUARVO", "NIMETTYTOTUUSARVO"]
        var paramFunctions = ["HAELUKUARVO", "LUKUARVO"]
        if(paramFunctions.indexOf(this.nimi) != -1) {
            return "parametri_template.html"
        } else if(labelFunctions.indexOf(this.nimi) != -1) {
            return "frame_template.html"
        } else {
            return "funktio_template.html"
        }
    }

    this.hasErrors = function() {
        return this.errors().length > 0
    }

    this.errors = function() {
        return this.data.validointivirheet || []
    }

    this.funktioCssClass = function(defaultClass, selected) {
        var cssClass = defaultClass
        if(selected != null && this.hashKey == selected.hashKey) {
            cssClass += " active"
        }
        if(this.data.deleted) {
            cssClass += " line-through"
        }
        return cssClass
    }

    this.iconCssClass = function() {
        if("SUMMA" == this.nimi) {
            return "icon sum"
        }
        return "icon"
    }

    this.isNimettava = function() {
        return NIMETTAVAT_TYYPPI.indexOf(this.nimi) != -1
    }

    this.getKonverteri = function() {
        var fdef = this.findFunctionDefinitionByFunktionimi(this.nimi)
        if(!fdef.konverteri) {
            return
        }
        return new Konverteri(fdef.konverteri, this.data)
    }

    this.addNew = function(funktionimi, argumenttiNimi) {
        var f = this.findFunctionDefinitionByFunktionimi(funktionimi)
        var newFunction = {
            child: {
                funktionimi: f.nimi,
                syoteparametrit: [],
                funktioargumentit: []
            }
        }
        for(var i in f.syoteparametrit) {
            var param = f.syoteparametrit[i]
            var newParam = {
                avain: param.avain,
                arvo: ""
            }
            newFunction.child.syoteparametrit.push(newParam)
        }

        // Arvotaan indeksi, johon data laitetaan
        // Case: Nimetty argumentti (jakolasku, suurempitaiyhtasuuri)
        if(argumenttiNimi) {
            var curFuncDef = this.findFunctionDefinitionByFunktionimi(this.nimi);
            var argumentti = curFuncDef.funktioargumentit.filter(function(arg) {
                return arg.nimi == argumenttiNimi
            })[0]
            var index = curFuncDef.funktioargumentit.indexOf(argumentti)
            newFunction.indeksi = index + 1
            this.data.funktioargumentit[index] = newFunction
            // Case: N argumenttia, lisätään listan loppuun.
        } else {
            newFunction.indeksi = this.data.funktioargumentit.length + 1
            this.data.funktioargumentit.push(newFunction)
        }

        this.funktioargumentit = this.getFunktioargumentit();
        return this.findFunktioArgumentti(newFunction.child)
    }

    this.addChildAt = function(funktio, index) {
        var data = {
            child: funktio.data
        }

        // Jos ei nimettyjä argumentteja, lisätään haluttuun indeksiin ja tuupataan muita yksi eteenpäin.
        if(!this.hasNimetytArgumentit()) {
            if(this.funktioargumentit.indexOf(funktio) !== -1) {
                return
            }
            data.indeksi = index + 1
            this.data.funktioargumentit.splice(index, 0, data)
            this.funktioargumentit = this.getFunktioargumentit()
            return
        }

        // Uudelleenjärjestäminen
        var oldIndex = this.funktioargumentit.indexOf(funktio)
        if(oldIndex !== -1) {
            this.funktioargumentit.splice(oldIndex, 1)
            if(this.funktioargumentit[index] instanceof TyhjaFunktio) {
                this.funktioargumentit[index] = funktio
            } else {
                this.funktioargumentit.splice(index, 0, funktio)
            }

            var fargs = []
            for(var i in this.funktioargumentit) {
                var f = this.funktioargumentit[i]
                if(f instanceof TyhjaFunktio) {
                    continue
                }
                data = {
                    child: f.data,
                    indeksi: parseInt(i) + 1
                }
                fargs.push(data)
            }
            this.data.funktioargumentit = fargs;
            this.funktioargumentit = this.getFunktioargumentit()
            return
        }

        // Etsitään edeltävä tyhjä funktio
        var fs = this.funktioargumentit.slice(0, index)
        var lastOf = -1
        for(var i = fs.length - 1; i >= 0; i--) {
            var f = fs[i]
            if(f instanceof TyhjaFunktio) {
                lastOf = i
                break
            }
        }

        // Korvataan tyhjä funktio, jos löytyi
        if(lastOf > -1) {
            data.indeksi = lastOf + 1
            this.data.funktioargumentit.splice(lastOf, 0, data)
        } else {
            // Tungetaan viimeisenä vaihtoehtona loppuun ja toivotaan toivotaan
            data.indeksi = this.data.funktioargumentit.length + 1
            this.data.funktioargumentit.push(data)
        }

        this.funktioargumentit = this.getFunktioargumentit()
    }

    this.removeChildFunktio = function(funktio) {
        var index = -1
        for (var i in this.data.funktioargumentit) {
            var farg = this.data.funktioargumentit[i]
            if(farg.child === funktio.data) {
                index = i
            }
        }
        if(index == -1) {
            return
        }

        var data = angular.copy(this.data.funktioargumentit[index].child)
        this.data.funktioargumentit.splice(index, 1)
        this.funktioargumentit = this.getFunktioargumentit()
        var func = new Funktio(angular.copy(this.dsl), data)
        func.init()
        return func
    }

    this.isHaettavaArvoTyyppi = function() {
        return HAETTAVA_TYYPPI.indexOf(this.nimi) !== -1
    }

    this.findFunktioArgumentti = function(data) {
        var args = this.funktioargumentit.filter(function(cur) {
            return angular.equals(cur.data, data)
        })
        return (args.length > 0 ? args[0] : undefined)
    }

    this.getAllErrors = function() {
        var errors = this.errors() || []
        for(var i in this.funktioargumentit) {
            var leafObj = this.funktioargumentit[i]
            if(leafObj instanceof Funktio) {
                var leafErrors = leafObj.getAllErrors()
                errors = errors.concat(leafErrors)
            }
        }
        return errors
    }
}

var Konverteri = function(konvDef, data) {
    this.konvDef = konvDef
    // this.data.arvovalikonverteriparametrit || this.data.arvokonverteriparametrit
    this.data = data
    this.oldData = []

    var TEMPLATE_MAP = {
        "ARVOKONVERTERI": "arvokonverteri-template",
        "ARVOVALIKONVERTERI": "arvovalikonverteri-template"
    }

    this.init = function() {
        this.tyyppi = this.getTyyppi()
        this.sallitut = this.getSallitut()
        this.parametrit = this.getParametrit()
        this.uusityyppi = this.tyyppi
    }

    this.getSallitut = function() {
        var sallitut = []
        for(var i in this.konvDef.konverteriTyypit) {
            var konvTyyppi = this.konvDef.konverteriTyypit[i]
            sallitut.push(konvTyyppi.tyyppi)
        }
        return sallitut
    }

    this.isSallittu = function(tyyppi) {
        return this.sallitut.indexOf(tyyppi) !== 1
    }

    this.getTyyppi = function() {
        if(this.konvDef.konverteriTyypit.length == 1) {
            return this.konvDef.konverteriTyypit[0]
        }
    }

    this.getParamIndex = function() {
        if(!this.tyyppi) {
            throw Error("Konverterillä ei ole tyyppiä")
        }
        return this.tyyppi == 'ARVOKONVERTERI' ? 'arvokonverteriparametrit' : 'arvovalikonverteriparametrit'
    }

    this.setTyyppi = function(tyyppi) {
        if(this.tyyppi) {
            var oldIdx = this.getParamIndex()
            this.oldData[this.tyyppi] = this.data[oldIdx].slice()
        }
        this.tyyppi = tyyppi
        this.template = TEMPLATE_MAP[tyyppi]
        var idx = this.getParamIndex()
        if(this.data[idx]) {
            this.data[idx].splice(0, this.data[idx].length)
        }
        if(this.oldData[this.tyyppi]) {
            for(var i = 0; i < this.oldData[this.tyyppi].length; i++) {
                this.data[idx].push(this.oldData[this.tyyppi][i])
            }
        }
        this.parametrit = this.getParametrit()
    }

    this.getDefinition = function() {
        var tyyppi = this.tyyppi
        return this.konvDef.konverteriTyypit.filter(function(cur) {
            return cur.tyyppi === tyyppi
        })[0]
    }

    this.getParametrit = function() {
        var konvparams = []
        if(!this.tyyppi) {
            return konvparams
        }
        var konv = this.getDefinition()
        var idx = this.getParamIndex()
        for(var i in this.data[idx]) {
            var konvparam = this.data[idx][i]
            konvparams.push(new KonverteriParametri(konv.arvotyyppi, konvparam))
        }
        /*this.data.konverteriparametrit = this.data || []
        if(this.data.konverteriparametrit.length == 0) {
            var data = {}
            this.data.konverteriparametrit.push(data)
            konvparams.push(new KonverteriParametri(konverteri.arvotyyppi, data))
        }*/
        return konvparams
    }

    this.addParametri = function() {
        var konverteri = this.getDefinition()
        if(!konverteri) {
            return
        }
        var data = {}
        var idx = this.getParamIndex()
        if(!this.data[idx]) {
            this.data[idx] = []
        }
        this.data[idx].push(data)
        this.parametrit.push(new KonverteriParametri(konverteri.arvotyyppi, data))
    }

    this.removeParametri = function(konvparam) {
        var idx = this.getParamIndex()
        var index = this.data[idx].indexOf(konvparam)
        if(index == -1) return
        this.data[idx].splice(index, 1)
        this.parametrit.splice(index, 1)
    }


    this.init()
}

var KonverteriParametri = function(tietotyyppi, data) {
    this.tietotyyppi = tietotyyppi
    this.data = data

    this.init = function() {

    }

    this.init()
}

var Parametri = function(definition, data) {
    this.definition = definition
    this.data = data

    /* Structure methods */
    this.avain = this.definition.avain
    //this.arvo = this.data ? this.data.arvo : null
    this.tyyppi = this.definition.tyyppi

    this.template = function() {
        if(this.tyyppi == "DESIMAALILUKU") {
            return "desimaaliluku-template"
        } else if (this.tyyppi == "KOKONAISLUKU") {
            return "kokonaisluku-template"
        }
        return ""
    }

    this.init = function() {
        this.hashKey = Math.random().toString(36).substring(7);
    }

    this.init()
}

app.factory('Laskentakaava', function($resource) {
    return $resource(SERVICE_URL_BASE + "resources/laskentakaava/:oid", {oid: "@oid"}, {
        list: {method: "GET", isArray: true},
        get: {method: "GET"},
        insert: {method: "PUT"},
        update: {method: "POST"}
    })
})
app.factory('FunktioKuvaus', function($resource) {
    return $resource(SERVICE_URL_BASE + "resources/laskentakaava/funktiokuvaus", {}, {
        get: {method: "GET", isArray: true}
    });
});

app.factory('KaavaValidointi', function($resource) {
    return $resource(SERVICE_URL_BASE + "resources/laskentakaava/validoi", {}, {
        post: {method: "POST"}
    })
})

app.factory('FunktioDSL', function($resource, FunktioKuvaus) {
    var model = [];
    var funktioLogic = {
        refresh: function() {
            FunktioKuvaus.get({}, function(result) {
                model = result;
            });
        },
        findByFunktionimi: function(funktionimi) {
            for(i in model) {
                var f = model[i];
                if(f.nimi == funktionimi) {
                    return f;
                }
            }
            throw Error("Function " + funktionimi + " not found.");
        }
    }
    return funktioLogic;
});

app.factory('Laskentapuu', function(Laskentakaava, FunktioKuvaus) {

    var model = []
    var kuvaus = {}

    var domainObject = {
        laskentakaava: function() {
            return model;
        },
        setKaavaData: function(data) {
            model[0] = new Kaava(kuvaus, data);
        },
        refresh: function(oid) {
            FunktioKuvaus.get({}, function(res) {
                kuvaus = res;
                Laskentakaava.get({oid: oid}, function(kaava) {
                    model = new Array(new Kaava(kuvaus, kaava))
                });
            });
        }
    }

    return domainObject;
});

function LaskentakaavaController($scope, $location, $routeParams, Laskentapuu, KaavaValidointi) {
    if(!$scope.fetched) {
        Laskentapuu.refresh($routeParams.laskentakaavaOid);
    }
    $scope.fetched = true;

    $scope.domain = Laskentapuu;
    $scope.showTemplate = false
    $scope.selected = null
    $scope.errors = []

    $scope.showDetails = function(funktio) {
        $scope.f = funktio
        $scope.showTemplate = true
        $scope.selected = funktio
    }

    $scope.addNewFunktio = function(parentFunktio, funktioNimi, argumenttiNimi) {
        var newFunc = parentFunktio.addNew(funktioNimi, argumenttiNimi);
        $scope.showDetails(newFunc);
    }

    $scope.kaavaDragged = function(funktio, oldParent, newParent, index) {
        var oldIndex = oldParent.funktioargumentit.indexOf(funktio)
        var func = funktio

        // Jos pudotetaan eri parenttiin, poistetaan vanhasta paikasta.
        if(oldParent !== newParent) {
            func = oldParent.removeChildFunktio(funktio)
        }
        newParent.addChildAt(func, index)
        newParent.init()
        oldParent.init()
    }

    $scope.saveKaavaAsCompleted = function() {
        var kaava = Laskentapuu.laskentakaava()[0].getData()
        KaavaValidointi.post({}, kaava, function(data) {
            Laskentapuu.setKaavaData(data)
            $scope.selected = null
            $scope.showTemplate = false

            if(Laskentapuu.laskentakaava()[0].hasErrors()) {
                $scope.errors = Laskentapuu.laskentakaava()[0].getAllErrors()
                return
            }

            kaava.onLuonnos = false
            kaava.$save({}, function(data) {

            })
            $scope.errors = []
        })

    }

    $scope.saveKaavaAsDraft = function() {
        var kaava = Laskentapuu.laskentakaava()[0].getData()
        kaava.onLuonnos = true
        kaava.$save({}, function(data) {
            Laskentapuu.setKaavaData(data)
            $scope.selected = null
            $scope.showTemplate = false
        })
    }

    $scope.goToListing = function() {
        $location.path("/laskentakaava")
    }
}

function LaskentakaavaListController($scope, $location, Laskentakaava) {
    $scope.laskentakaavat = Laskentakaava.list({all: true})
    $scope.showForm = false
    $scope.kaava = {
        tyyppi: "NIMETTYLUKUARVO"
    }

    $scope.createKaava = function(kaavaData) {

        var kaava = {
            onLuonnos: true,
            nimi: kaavaData.nimi,
            funktiokutsu: {
                funktionimi: kaavaData.tyyppi,
                syoteparametrit: [
                    {
                        avain: "nimi",
                        arvo: kaavaData.nimi
                    }
                ]
            }
        }

        Laskentakaava.insert({}, kaava, function(result) {
            $location.path("/laskentakaava/" + result.id)
        })
    }
}