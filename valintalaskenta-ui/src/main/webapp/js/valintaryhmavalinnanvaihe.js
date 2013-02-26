
app.factory('ValintaryhmaValinnanvaiheModel', function($resource, $location, $routeParams, Valintaryhma, Valinnanvaihe, Valintatapajono, ValinnanvaiheValintatapajono, NewValintaryhmaValinnanvaihe) {
    
    var model = new function() {
        
        this.valinnanvaihe = {};
        this.valintatapajonot = [];

        this.refresh = function (oid) {
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
        };
        this.refreshIfNeeded = function(oid) {
            if(oid !== model.valinnanvaihe.oid) {
                model.refresh(oid)
            }
        };
        this.persistValintaryhmaValinnanvaihe = function(parentValintaryhmaOid, valinnanvaiheet) {
            if(model.valinnanvaihe.oid) {
                
                Valinnanvaihe.post(model.valinnanvaihe, function(result) {
                    var i;
                    for(i in valinnanvaiheet) {
                        if(result.oid === valinnanvaiheet[i].oid) {
                            valinnanvaiheet[i] = result;
                        }
                    }
                });
                
                model.valintatapajonot.forEach(function(element, index, array){
                    Valintatapajono.post({oid: model.valintatapajonot[index].oid}, element, function(result) {
                        model.valintatapajonot[index] = result;
                    });
                });
                
            } else {
                var valinnanvaihe = {
                    nimi: model.valinnanvaihe.nimi,
                    kuvaus: model.valinnanvaihe.kuvaus,
                    aktiivinen: true
                }
                NewValintaryhmaValinnanvaihe.put({valintaryhmaOid: parentValintaryhmaOid}, valinnanvaihe, function(result){
                    model.valinnanvaihe = result;
                    valinnanvaiheet.push(result);
                });
            }
        };
        this.remove = function(jono) {
            
            Valintatapajono.delete({oid: jono.oid}, function(result) {    
                model.refresh(model.valinnanvaihe.oid);
            })
        }

    }
    return model;

});

function valintaryhmaValinnanvaiheController($scope, $location, $routeParams, Valintaryhma, ValintaryhmaValinnanvaiheModel, ChildValintaryhmas, NewHakukohde, ChildHakukohdes, ValintaryhmaModel) {
    $scope.valintaryhmaOid = $routeParams.id;
    $scope.ValintaryhmaValinnanvaiheOid = $routeParams.valinnanvaiheOid;
    $scope.model = ValintaryhmaValinnanvaiheModel;
    $scope.model.refreshIfNeeded($scope.ValintaryhmaValinnanvaiheOid);

    $scope.submit = function() {
        $scope.model.persistValintaryhmaValinnanvaihe($scope.valintaryhmaOid, ValintaryhmaModel.valinnanvaiheet);
    }

    $scope.cancel = function() {
        $location.path("/valintaryhma/" + $scope.valintaryhmaOid);
    }

    $scope.addJono = function() {
        $location.path("/valintaryhma/" + $scope.valintaryhmaOid + "/valinnanvaihe/" + $scope.model.valinnanvaihe.oid + "/valintatapajono/");
    }

    $scope.modifyJono = function(oid) {
        $location.path("/valintaryhma/" + $scope.valintaryhmaOid + "/valinnanvaihe/" + $scope.model.valinnanvaihe.oid + "/valintatapajono/" + oid);
    }

}

