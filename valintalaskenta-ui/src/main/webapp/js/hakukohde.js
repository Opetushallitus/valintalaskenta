

app.factory('HakukohdeModel', function($resource, Hakukohde, Valintaryhma, HakukohdeValinnanvaihe, Valinnanvaihe, ValinnanvaiheJarjesta) {
    var model = new function()  {
        
        this.parentValintaryhma = {};
        this.hakukohde = {};
        this.valinnanvaiheet = [];

        this.refresh = function(oid) {
            Hakukohde.get({oid: oid}, function(result) {
                model.hakukohde = result;
                
                Valintaryhma.get({oid: model.hakukohde.valintaryhma_id}, function(result) {
                    model.parentValintaryhma = result;
                });
            });

            model.refreshValinnanvaiheet(oid);
        };
        this.refreshIfNeeded = function(oid) {
          if(oid != model.hakukohde.oid) {
             this.refresh(oid);
          }

        };

        this.refreshValinnanvaiheet = function(oid) {
            HakukohdeValinnanvaihe.get({parentOid: oid}, function(result) {
                model.valinnanvaiheet = result;
            });
        };

        this.persistHakukohde = function() {
            Hakukohde.post(model.hakukohde, function(result) {});
            if(model.valinnanvaiheet.length > 0) {
                ValinnanvaiheJarjesta.post(getValinnanvaiheOids(), function(result) {});
                for(var i = 0 ; i < model.valinnanvaiheet.length ; ++i) {
                    Valinnanvaihe.post(model.valinnanvaiheet[i], function(){});
                }
            }
        };
        this.remove = function(vaihe) {
            Valinnanvaihe.delete({oid: vaihe.oid} ,function(result) {
                for(i in model.valinnanvaiheet) {
                    if(vaihe.oid === model.valinnanvaiheet[i].oid) {
                        model.valinnanvaiheet.splice(i,1);
                    }
                }
            });
        };

    };

    function getValinnanvaiheOids() {
        var oids = [];
        for (var i = 0 ; i < model.valinnanvaiheet.length ; ++i) {
            oids.push(model.valinnanvaiheet[i].oid);
        }
        return oids;
    }

    return model;
});



function HakukohdeController($scope, $location, $routeParams, HakukohdeModel) {
    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
    $scope.model = HakukohdeModel;
    $scope.model.refreshIfNeeded($scope.hakukohdeOid);

    $scope.submit = function() {
        $scope.model.persistHakukohde();
    }
    $scope.cancel = function() {
        $location.path("/");
    }
    $scope.lisaaValinnanVaihe = function() {
        $location.path("/hakukohde/" + $scope.hakukohdeOid + "/valinnanvaihe/");
    }
}

function HakukohdeLaskentakaavaController($scope, $location, $routeParams, Laskentakaava) {
     $scope.hakukohdeOid = $routeParams.hakukohdeOid;
}








app.factory('HakukohdeCreatorModel', function($resource, Hakukohde, Valintaryhma, HakukohdeValinnanvaihe, Valinnanvaihe, ValinnanvaiheJarjesta) {
    var model = new function()  {
        this.hakukohde = {};

        this.refresh = function() {
            model.hakukohde = {};
        }

        this.persistHakukohde = function(valintaryhmaOid) {
            model.hakukohde.valintaryhmaOid = valintaryhmaOid;
            Hakukohde.insert(model.hakukohde, function(result) {});
        }

    }

    return model;
});

function HakukohdeCreatorController($scope, $location, $routeParams, HakukohdeCreatorModel) {
    $scope.valintaryhmaOid = $routeParams.valintaryhmaOid;
    $scope.model = HakukohdeCreatorModel;

    $scope.cancel = function() {
        $location.path("/");
    };

    $scope.submit = function() {
        $scope.model.persistHakukohde($scope.valintaryhmaOid);
    }
}