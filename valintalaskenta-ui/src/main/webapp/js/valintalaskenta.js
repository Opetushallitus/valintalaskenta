var app = angular.module('valintalaskenta', ['ngResource', 'loading']);

var SERVICE_URL_BASE = SERVICE_URL_BASE || ""
var TEMPLATE_URL_BASE = TEMPLATE_URL_BASE || ""

//Route configuration
app.config(function($routeProvider) {
        $routeProvider.

        when('/etusivu', {redirectTo:'/haku//hakukohde/valinnanhallinta'}).

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


//Valintaryhma
app.factory('RootValintaryhmas', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valintaryhma", {paataso: true}, {
    get: {method: "GET", isArray: true}
  });
});
app.factory('ChildValintaryhmas', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valintaryhma/:parentOid/lapsi", {parentOid: "@parentOid"}, {
    get: {method: "GET", isArray: true},
    insert: {method: "PUT"}
  });
});
app.factory('ChildHakukohdes', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valintaryhma/:oid/hakukohde", {}, {
    get: {method: "GET", isArray: true}
  });
});
app.factory('Valintaryhma', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valintaryhma/:oid", {oid: "@oid"}, {
    get: {method: "GET"}  ,
    post:{method: "POST"},
    insert: {method: "PUT"}
  });
});

app.factory('ValintaryhmaValinnanvaihe', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valintaryhma/:oid/valinnanvaihe", {oid: "@oid"}, {
    get: {method: "GET", isArray: true}
  });
});
app.factory('NewValintaryhmaValinnanvaihe', function($resource) {
return $resource(SERVICE_URL_BASE + "resources/valintaryhma/:valintaryhmaOid/valinnanvaihe", {valintaryhmaOid: "@valintaryhmaOid"}, {
    put: {method: "PUT"}
  });
});