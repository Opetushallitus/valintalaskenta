


function ValinnanVaiheController($scope, $location, $routeParams, HakukohdeValinnanVaiheModel, HakukohdeModel) {
    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
    $scope.valinnanvaiheOid = $routeParams.valinnanvaiheOid;

    $scope.model = HakukohdeValinnanVaiheModel;
    $scope.model.refreshIfNeeded($scope.valinnanvaiheOid);

    $scope.submit = function() {
        $scope.model.persistValinnanvaihe($scope.hakukohdeOid, HakukohdeModel.valinnanvaiheet);
    }

    $scope.cancel = function() {
        $location.path("/hakukohde/" + $scope.hakukohdeOid);
    }

    $scope.addJono = function() {
        $location.path("/hakukohde/" + $scope.hakukohdeOid + "/valinnanvaihe/" + $scope.model.valinnanvaihe.oid + "/valintatapajono/");
    }

    $scope.modifyJono = function(oid) {
        $location.path("/hakukohde/" + $scope.hakukohdeOid + "/valinnanvaihe/" + $scope.model.valinnanvaihe.oid + "/valintatapajono/" + oid);
    }
}

app.factory('HakukohdeValinnanVaiheModel', function($resource, $location, Valinnanvaihe, Valintatapajono, ValinnanvaiheValintatapajono, HakukohdeValinnanvaihe) {
    var model = new function() {
        this.valinnanvaihe = {};
        this.valintatapajonot = [];


        this.refresh = function(oid) {
            if(!oid) {
                model.valinnanvaihe = {};
                model.valintatapajonot = [];
            } else {
                
                Valinnanvaihe.get({oid: oid}, function(result) {
                    model.valinnanvaihe = result;  
                });
                
                ValinnanvaiheValintatapajono.get({parentOid: oid}, function(result) {
                    model.valintatapajonot = result;
                }); 
            }
        }

        this.refreshIfNeeded = function(oid) {
            if( oid !== model.valinnanvaihe.oid ) {
                this.refresh(oid);
            }
        };

        this.remove = function(jono) {
            Valintatapajono.delete({oid: jono.oid}, function(result) {
                for(i in model.valintatapajonot) {
                    if(jono.oid === model.valintatapajonot[i].oid) {
                        model.valintatapajonot.splice(i,1);
                    }
                }
            });
        };

        this.persistValinnanvaihe = function(hakukohdeParentOid, valinnanvaiheet) {
            if(!model.valinnanvaihe.oid) {
                model.valinnanvaihe.aktiivinen = true;
                HakukohdeValinnanvaihe.insert({"parentOid": hakukohdeParentOid}, model.valinnanvaihe, function(result) {
                    model.valinnanvaihe = result;
                    valinnanvaiheet.push(result);
                });
            } else {
                Valinnanvaihe.post(model.valinnanvaihe, function(result) {
                    model.valinnanvaihe = result;
                    var i;
                    for(i in valinnanvaiheet) {
                        if(result.oid === valinnanvaiheet[i].oid) {
                            valinnanvaiheet[i] = result;
                        }
                    }
                });

                for(var i = 0; i < model.valintatapajonot.length ; ++i) {
                    Valintatapajono.post(model.valintatapajonot[i], function(result) {});
                }
            }
        };
    }
    return model;
});