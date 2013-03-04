var app = angular.module('valintalaskenta', ['ngResource', 'loading']);

var SERVICE_URL_BASE = SERVICE_URL_BASE || ""
var TEMPLATE_URL_BASE = TEMPLATE_URL_BASE || ""

//Route configuration
app.config(function($routeProvider) {
        $routeProvider.

        when('/etusivu', {redirectTo:'/haku//hakukohde//valinnanhallinta'}).

        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/valinnanhallinta', {controller:ValinnanhallintaController, templateUrl:TEMPLATE_URL_BASE + 'valinnanhallinta.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/pistesyotto', {controller:PistesyottoController, templateUrl:TEMPLATE_URL_BASE + 'pistesyotto.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/harkinnanvaraiset/pistelaskennassa', {controller:PistelaskentaController, templateUrl:TEMPLATE_URL_BASE + 'pistelaskennassa.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/harkinnanvaraiset/harkinnassa', {controller:HarkinnassaController, templateUrl:TEMPLATE_URL_BASE + 'harkinnassa.html'}).
        when('/haku/:hakuOid/hakukohde/:hakukohdeOid/valintaesitys', {controller:ValintaesitysController, templateUrl:TEMPLATE_URL_BASE + 'valintaesitys.html'}).

        //when('/haku/:hakuOid/henkiloittain', {controller:HenkiloController, templateUrl:TEMPLATE_URL_BASE + 'henkiloittain.html'}).
        //when('/haku/:hakuOid/henkiloittain/mitätuleekin', {controller:HenkiloController, templateUrl:TEMPLATE_URL_BASE + 'henkiloittain.html'}).


        when('/haku/:hakuOid/yhteisvalinnanhallinta', {controller:YhteisvalinnanHallintaController, templateUrl:TEMPLATE_URL_BASE + 'yhteisvalinnanhallinta.html'}).

        otherwise({redirectTo:'/etusivu'});


//        //front page
//        when('/etusivu', {controller:ValintaesitysController, templateUrl:TEMPLATE_URL_BASE + 'hakukohde.html'}).
//		when('/hakukohde:id', {controller:ValintaesitysController, templateUrl:TEMPLATE_URL_BASE + 'hakukohde.html'}).
//		when('/valintaesitys', {controller:ValintaesitysController, templateUrl:TEMPLATE_URL_BASE + 'valintaesitys.html'}).
//		when('/harkinnanvaraisethakijat', {controller:ValintaesitysController, templateUrl:TEMPLATE_URL_BASE + 'harkinnanvaraisethakijat.html'}).
//		when('/pistesyotto', {controller:ValintaesitysController, templateUrl:TEMPLATE_URL_BASE + 'pistesyotto.html'}).
//		when('/valinnanhallinta', {controller:ValintaesitysController, templateUrl:TEMPLATE_URL_BASE + 'valinnanhallinta.html'}).
//
//        //else
//        otherwise({redirectTo:'/etusivu'});
});


//rest resources


//Haku
app.factory('Haku', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/haku/", {}, {
    get: {method: "GET", isArray: true}
  });
});

app.factory('HakuHakukohdeChildren', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/haku/:hakuOid/hakukohde", {hakuOid: "@hakuOid"}, {
    get: {method: "GET", isArray: true}
  });
});


//Hakukohde
app.factory('Hakukohde', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/hakukohde/:hakukohdeoid/valinnanvaihe", {hakukohdeoid: "@hakukohdeoid"}, {
    get: {method: "GET", isArray: true}
  });
});


//Valintatapajono
app.factory('Valintatapajono', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valintatapajono/:valintatapajonoid/jarjestyskriteeritulos", {valintatapajonoid: "@valintatapajonoid"}, {
    get: {method: "GET", isArray: true}
  });
});


//Valinnanvaihe
app.factory('Valinnanvaihe', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valinnanvaihe/:valinnanvaiheoid/valintatapajono", {valinnanvaiheoid: "@valinnanvaiheoid"}, {
    get: {method: "GET", isArray: true}
  });
});


