//domain .. this is both, service & domain layer
app.factory('ValintatapajonoModel', function($q, Valintatapajono, ValinnanvaiheValintatapajono,
                                                ValintatapajonoJarjestyskriteeri, Laskentakaava, Jarjestyskriteeri,
                                                JarjestyskriteeriJarjesta) {
    var model = new function()  {
        this.valintatapajono = {};
        this.jarjestyskriteerit = [];

        this.refresh = function(oid) {

            Valintatapajono.get({oid: oid}, function(result) {
                model.valintatapajono = result;
            });

            this.refreshJK(oid);
        };

        this.refreshIfNeeded = function(oid) {
            if(!oid) {
                model.valintatapajono = {};
                model.jarjestyskriteerit = [];
                model.valintatapajono.tasapistesaanto = "YLITAYTTO";
            } else if (oid != model.valintatapajono.oid) {
                this.refresh(oid);
            }
        };

        this.refreshJK = function(oid) {
            ValintatapajonoJarjestyskriteeri.get({parentOid: oid}, function(result) {
                model.jarjestyskriteerit = result;
                model.jarjestyskriteerit.forEach(function(jk){
                    Laskentakaava.get({oid: jk.laskentakaava_id}, function(result) {
                        jk.nimi = result.nimi;
                    });
                });
            });
        };

        this.submit = function(valinnanvaiheOid, valintatapajonot) {
            if(model.valintatapajono.oid == null) {
                model.valintatapajono.aktiivinen = true;
                ValinnanvaiheValintatapajono.insert({parentOid: valinnanvaiheOid}, model.valintatapajono,
                function(result) {
                    model.valintatapajono = result;
                    valintatapajonot.push(result);
                });
            } else {
                Valintatapajono.post(model.valintatapajono, function(result) {
                    var i;
                    for(i in valintatapajonot) {
                        if(result.oid === valintatapajonot[i].oid) {
                            valintatapajonot[i] = result;
                        }
                    }
                    model.valintatapajono = result;

                });

                var promises = [];
                for(var i = 0 ; i < model.jarjestyskriteerit.length ; ++i) {
                    promises[i] = function(){
                        var deferred = $q.defer();
                        Jarjestyskriteeri.post(model.jarjestyskriteerit[i], function(result){
                            deferred.resolve();
                        });
                        return deferred.promise;
                    }();
                }

                $q.all(promises).then(function(){
                    jarjestaJarjestyskriteerit();
                });
            }
        };

        this.remove = function(oid) {
            Jarjestyskriteeri.delete({oid:oid}, function() {
                for(i in model.jarjestyskriteerit) {
                    if(oid === model.jarjestyskriteerit[i].oid) {
                        model.jarjestyskriteerit.splice(i,1);
                    }
                }
            });
        };

        function jarjestaJarjestyskriteerit() {
            if(model.jarjestyskriteerit.length > 0) {
                JarjestyskriteeriJarjesta.post(getJarjestyskriteeriOids(), function(result) {
                    model.jarjestyskriteerit = result;
                    model.jarjestyskriteerit.forEach(function(jk){
                        Laskentakaava.get({oid: jk.laskentakaava_id}, function(result) {
                            jk.nimi = result.nimi;
                        });
                    });
                });
            }
        };

        function getJarjestyskriteeriOids() {
            var oids = [];
            for (var i = 0 ; i < model.jarjestyskriteerit.length ; ++i) {
                oids.push(model.jarjestyskriteerit[i].oid);
            }
            return oids;
        }
    };


    return model;
});

function HakukohdeValintatapajonoController($scope, $location, $routeParams, $q, ValintatapajonoModel, HakukohdeValinnanVaiheModel) {

    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
    $scope.valinnanvaiheOid = $routeParams.valinnanvaiheOid;
    //$scope.valintatapajonoOid =  $routeParams.valintatapajonoOid;

    $scope.model = ValintatapajonoModel;
    $scope.model.refreshIfNeeded($routeParams.valintatapajonoOid);



    $scope.submit = function() {
        $scope.model.submit($scope.valinnanvaiheOid, HakukohdeValinnanVaiheModel.valintatapajonot);
    }

    $scope.cancel = function() {
        $location.path("/hakukohde/" + $scope.hakukohdeOid + '/valinnanvaihe/'+ $scope.valinnanvaiheOid );
    }

    $scope.addKriteeri = function() {
        $location.path("/hakukohde/" + $scope.hakukohdeOid
            + '/valinnanvaihe/' + $scope.valinnanvaiheOid
            + '/valintatapajono/' + $scope.model.valintatapajono.oid + '/jarjestyskriteeri/');
    }

    $scope.modifyKriteeri = function(oid) {
        $location.path("/hakukohde/" + $scope.hakukohdeOid
                    + '/valinnanvaihe/' + $scope.valinnanvaiheOid
                    + '/valintatapajono/' + $scope.model.valintatapajono.oid
                    + '/jarjestyskriteeri/' + oid);
    }

    $scope.remove = function(oid) {
        $scope.model.remove(oid);
    }
}



function ValintaryhmaValintatapajonoController($scope, $location, $routeParams, $q, ValintatapajonoModel, ValintaryhmaValinnanvaiheModel) {

    $scope.valintaryhmaOid = $routeParams.id;
    $scope.valinnanvaiheOid = $routeParams.valinnanvaiheOid;
    //$scope.valintatapajonoOid =  $routeParams.valintatapajonoOid;

    $scope.model = ValintatapajonoModel;
    $scope.model.refreshIfNeeded($routeParams.valintatapajonoOid);



    $scope.submit = function() {
        $scope.model.submit($scope.valinnanvaiheOid, ValintaryhmaValinnanvaiheModel.valintatapajonot);
    }

    $scope.cancel = function() {
        $location.path("/valintaryhma/" + $scope.valintaryhmaOid + '/valinnanvaihe/'+ $scope.valinnanvaiheOid );
    }

    $scope.addKriteeri = function() {
        $location.path("/valintaryhma/" + $scope.valintaryhmaOid
            + '/valinnanvaihe/' + $scope.valinnanvaiheOid
            + '/valintatapajono/' + $scope.model.valintatapajono.oid + '/jarjestyskriteeri/');
    }

    $scope.modifyKriteeri = function(oid) {
        $location.path("/valintaryhma/" + $scope.valintaryhmaOid
                    + '/valinnanvaihe/' + $scope.valinnanvaiheOid
                    + '/valintatapajono/' + $scope.model.valintatapajono.oid
                    + '/jarjestyskriteeri/' + oid);
    }

    $scope.remove = function(oid) {
        $scope.model.remove(oid);
    }

}