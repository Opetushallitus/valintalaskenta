var app = angular.module('valintalaskenta', ['ngResource', 'loading']);

var SERVICE_URL_BASE = SERVICE_URL_BASE || "";
var TEMPLATE_URL_BASE = TEMPLATE_URL_BASE || "";
var VALINTAPERUSTEET_URL_BASE = VALINTAPERUSTEET_URL_BASE || "";
var VALINTALASKENTAKOOSTE_URL_BASE = VALINTALASKENTAKOOSTE_URL_BASE || "";
var TARJONTA_URL_BASE = TARJONTA_URL_BASE || "";


//Route configuration
app.config(function($routeProvider) {
        $routeProvider.

        
        when('/haku/:hakuOid/hakukohde/', {controller:HakukohdeController, templateUrl:TEMPLATE_URL_BASE + 'hakukohde.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/perustiedot', {controller:HakukohdeController, templateUrl:TEMPLATE_URL_BASE + 'hakukohdeperustiedot.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/valinnanhallinta', {controller:ValinnanhallintaController, templateUrl:TEMPLATE_URL_BASE + 'valinnanhallinta.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/pistesyotto', {controller:PistesyottoController, templateUrl:TEMPLATE_URL_BASE + 'pistesyotto.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/harkinnanvaraiset/pistelaskennassa', {controller:PistelaskentaController, templateUrl:TEMPLATE_URL_BASE + 'pistelaskennassa.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/harkinnanvaraiset/harkinnassa', {controller:HarkinnassaController, templateUrl:TEMPLATE_URL_BASE + 'harkinnassa.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/valintaesitys', {controller:ValintaesitysController, templateUrl:TEMPLATE_URL_BASE + 'valintaesitys.html'}).


        //when('/haku/:hakuOid/henkiloittain', {controller:HenkiloController, templateUrl:TEMPLATE_URL_BASE + 'henkiloittain.html'}).
        //when('/haku/:hakuOid/henkiloittain/mitätuleekin', {controller:HenkiloController, templateUrl:TEMPLATE_URL_BASE + 'henkiloittain.html'}).


        when('/haku/:hakuOid/yhteisvalinnanhallinta', {controller:YhteisvalinnanHallintaController, templateUrl:TEMPLATE_URL_BASE + 'yhteisvalinnanhallinta.html'}).

        otherwise({redirectTo:'/haku//hakukohde/'});
});

 
//rest resources
app.factory('Haku', function($resource) {
  return $resource(TARJONTA_URL_BASE + "haku", {}, {
    get: {method: "GET", isArray: true}
  });
});


app.factory('HakuHakukohdeChildren', function($resource) {
return $resource(TARJONTA_URL_BASE + "haku/:hakuOid/hakukohde", {hakuOid: "@hakuOid"}, {
    get: {method: "GET", isArray: true}
  });
});

app.factory('tarjontaHakukohde', function($resource) {
return $resource(TARJONTA_URL_BASE + "hakukohde/:hakukohdeoid", {hakukohdeoid: "@hakukohdeoid"}, {
    get: {method: "GET"}
  });
});

//One does not simply call 'ValinnanVaiheList' 'Hakukohde'
app.factory('ValinnanvaiheListByHakukohde', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/hakukohde/:hakukohdeoid/valinnanvaihe", {hakukohdeoid: "@hakukohdeoid"}, {
    get: {method: "GET", isArray: true}
  });
});

//One does not simply call 'ValinnanVaiheList' 'Hakukohde'
app.factory('ValinnanvaiheListFromValintaperusteet', function($resource) {
    return $resource(VALINTAPERUSTEET_URL_BASE + "resources/hakukohde/:hakukohdeoid/valinnanvaihe", {hakukohdeoid: "@hakukohdeoid"}, {
        get: {method: "GET", isArray: true}
    });
});


// Järjestyskriteeritulokset
app.factory('Valintatapajono', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valintatapajono/:valintatapajonoid/jarjestyskriteeritulos", {valintatapajonoid: "@valintatapajonoid"}, {
    get: {method: "GET", isArray: true}
  });
});


//Valintatapajonot
app.factory('ValintatapajonoListByValinnanvaihe', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valinnanvaihe/:valinnanvaiheoid/valintatapajono", {valinnanvaiheoid: "@valinnanvaiheoid"}, {
    get: {method: "GET", isArray: true}
  });
});

app.factory('ValintalaskentaAktivointi', function($resource) {
    return $resource(VALINTALASKENTAKOOSTE_URL_BASE + "resources/valintalaskenta/aktivoi", {}, {
        aktivoi: {method: "GET"}
    })
});

app.factory('KaikkiHakemukset', function($resource) {
    return $resource(HAKEMUS_URL_BASE + "applications/", {}, {
        get: {method: "GET", isArray: true}
    });
});

app.factory('Hakemus', function($resource) {
    return $resource(HAKEMUS_URL_BASE + "applications/:oid", {oid: "@oid"}, {
        get: {method: "GET"}
    });
});

app.factory('HakemusKey', function($resource) {
    return $resource(HAKEMUS_URL_BASE + "applications/:oid/:key", {oid: "@oid", key: "@key"}, {
        get: {method: "GET"},
        put: {method: "PUT"}
    });
});