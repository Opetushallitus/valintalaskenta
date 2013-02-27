var app = angular.module('valintalaskenta', ['ngResource', 'loading']);

var SERVICE_URL_BASE = SERVICE_URL_BASE || ""
var TEMPLATE_URL_BASE = TEMPLATE_URL_BASE || ""

//Route configuration
app.config(function($routeProvider) {
        $routeProvider.

        //front page
        when('/etusivu', {controller:ValintaController, templateUrl:TEMPLATE_URL_BASE + 'hakukohde.html'}).
		when('/hakukohde', {controller:ValintaController, templateUrl:TEMPLATE_URL_BASE + 'hakukohde.html'}).
		when('/valintaesitys', {controller:ValintaController, templateUrl:TEMPLATE_URL_BASE + 'valintaesitys.html'}).
		when('/harkinnanvaraisethakijat', {controller:ValintaController, templateUrl:TEMPLATE_URL_BASE + 'harkinnanvaraisethakijat.html'}).
		when('/pistesyotto', {controller:ValintaController, templateUrl:TEMPLATE_URL_BASE + 'pistesyotto.html'}).
		when('/valinnanhallinta', {controller:ValintaController, templateUrl:TEMPLATE_URL_BASE + 'valinnanhallinta.html'}).
		
        //else
        otherwise({redirectTo:'/etusivu'});
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
