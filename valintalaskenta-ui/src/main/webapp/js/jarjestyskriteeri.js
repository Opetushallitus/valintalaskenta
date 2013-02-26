
app.factory('JarjestyskriteeriModel', function($q, Laskentakaava, Laskentakaavat, Jarjestyskriteeri, ValintatapajonoJarjestyskriteeri) {
    var model;

    model = new function() {
        this.jarjestyskriteeri = {};
        this.laskentakaavat = [];

        this.refresh = function(oid) {
            if(oid) {
                Jarjestyskriteeri.get({oid: oid}, function(result) {
                    model.jarjestyskriteeri = result;
                });
            }
        };

        this.refreshIfNeeded = function(oid) {
            if(!oid) {
                model.jarjestyskriteeri = {};
                model.laskentakaavat = [];
            } else if (oid !== model.jarjestyskriteeri.oid) {
                this.refresh(oid);
            }

            Laskentakaavat.get(function(result) {
                model.laskentakaavat = result;
                if(!model.jarjestyskriteeri.laskentakaava_id) {
                    model.jarjestyskriteeri.laskentakaava_id = model.laskentakaavat[0].id;
                }
            });
        };

        this.submit = function(valintatapajonoOid, jarjestyskriteerit) {
            var deferred = $q.defer();
            if(model.jarjestyskriteeri.oid == null) {
                model.jarjestyskriteeri.aktiivinen = "true";
                ValintatapajonoJarjestyskriteeri.insert({parentOid: valintatapajonoOid}, model.jarjestyskriteeri,
                    function(jk) {
                        Laskentakaava.get({oid: jk.laskentakaava_id}, function(result) {
                            jk.nimi = result.nimi;
                        });
                        jarjestyskriteerit.push(jk);
                        deferred.resolve();
                });
            } else {
                Jarjestyskriteeri.post(model.jarjestyskriteeri, function(jk) {
                    var i;

                    for(i in jarjestyskriteerit) {
                        if(jk.oid === jarjestyskriteerit[i].oid) {

                            Laskentakaava.get({oid: jk.laskentakaava_id}, function(result) {
                                jk.nimi = result.nimi;
                            });

                            jarjestyskriteerit[i] = jk;
                            deferred.resolve();
                        }
                    }
                });
            }

            return deferred.promise;
        };
    };

    return model;
});

function JarjestyskriteeriController($scope, $location, $routeParams, JarjestyskriteeriModel, ValintatapajonoModel) {

//    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
    $scope.valinnanvaiheOid = $routeParams.valinnanvaiheOid;
    $scope.valintatapajonoOid =  $routeParams.valintatapajonoOid;
//    $scope.jarjestyskriteeriOid =  $routeParams.jarjestyskriteeriOid;

    $scope.model = JarjestyskriteeriModel;
    $scope.model.refreshIfNeeded($routeParams.jarjestyskriteeriOid);

    $scope.submit = function() {
        var promise = JarjestyskriteeriModel.submit($scope.valintatapajonoOid, ValintatapajonoModel.jarjestyskriteerit);
        promise.then(function(greeting) {
            var path;
            if($routeParams.hakukohdeOid) {
                path = "/hakukohde/" + $routeParams.hakukohdeOid;
            } else {
                path = "/valintaryhma/" + $routeParams.id;
            }
            $location.path(path + '/valinnanvaihe/'+ $routeParams.valinnanvaiheOid +
                                    '/valintatapajono/' + $routeParams.valintatapajonoOid);
        });

    };

    $scope.cancel = function() {
        var path;
        if($routeParams.hakukohdeOid) {
            path = "/hakukohde/" + $routeParams.hakukohdeOid;
        } else {
            path = "/valintaryhma/" + $routeParams.id;
        }
        $location.path(path + '/valinnanvaihe/'+ $routeParams.valinnanvaiheOid +
                                '/valintatapajono/' + $routeParams.valintatapajonoOid);
    };

}