
app.factory('ValintaryhmaModel', function(Valintaryhma, ChildValintaryhmas, ChildHakukohdes, Valinnanvaihe, ValintaryhmaValinnanvaihe, Treemodel, ValinnanvaiheJarjesta) {

    var model = new function() {
        this.valintaryhma = {};
        this.valinnanvaiheet =[];

        this.refresh = function(oid) {
            if(!oid) {
                model.valintaryhma = {};
                model.valinnanvaiheet = [];
            } else {
                Valintaryhma.get({oid: oid}, function(result) {
                    model.valintaryhma = result;
                });

                ValintaryhmaValinnanvaihe.get({oid: oid}, function(result) {
                    model.valinnanvaiheet = result;
                });

            }
        };

        this.refreshIfNeeded = function(oid) {
            if( oid !== model.valintaryhma.oid ) {
                this.refresh(oid);
            }
        };

        this.persistValintaryhma = function(oid) {
            Valintaryhma.post(model.valintaryhma, function(result) {
                model.valintaryhma = result;
                Treemodel.refresh();
            });

            if(model.valinnanvaiheet.length > 0) {
                ValinnanvaiheJarjesta.post(getValinnanvaiheOids(), function(result) {});
                for(var i = 0 ; i < model.valinnanvaiheet.length ; ++i) {
                    Valinnanvaihe.post(model.valinnanvaiheet[i], function(){});
                }
            }
        };

        this.removeValinnanvaihe = function(vaihe) {
            Valinnanvaihe.delete({oid: vaihe.oid}, function(){
                for(i in model.valinnanvaiheet) {
                    if(vaihe.oid === model.valinnanvaiheet[i].oid) {
                        model.valinnanvaiheet.splice(i,1);
                    }
                }
            });
        }

        function getValinnanvaiheOids() {
            var oids = [];
            for (var i = 0 ; i < model.valinnanvaiheet.length ; ++i) {
                oids.push(model.valinnanvaiheet[i].oid);
            }
            return oids;
        }

    }

    return model;
});

function valintaryhmaController($scope, $location, $routeParams, ValintaryhmaModel) {
    $scope.valintaryhmaOid = $routeParams.id;
    $scope.model = ValintaryhmaModel;
    $scope.model.refreshIfNeeded($scope.valintaryhmaOid);

    $scope.submit = function() {
        $scope.model.persistValintaryhma($scope.valintaryhmaOid);
    }

    $scope.cancel = function() {
        $location.path("/");
    }

    $scope.lisaaValinnanVaihe = function() {
        $location.path("/valintaryhma/" + $scope.valintaryhmaOid + "/valinnanvaihe/");
    }

    $scope.toValintaryhmaForm = function() {
        $location.path("/valintaryhma/" + $scope.valintaryhmaOid);
    }
}








app.factory('ValintaryhmaCreatorModel', function($resource, $location, $routeParams, Valintaryhma, ChildValintaryhmas, Treemodel ) {

    var model = new function() {
        this.valintaryhma = {};

        this.refresh = function() {
            model.valintaryhma = {};
        };

        this.refreshIfNeeded = function() {
            this.refresh();
        };

        this.persistValintaryhma = function(oid) {
            
            var newValintaryhma = {
                hakuOid: 1,
                lapsihakukohde: false,
                lapsivalintaryhma: false,
                nimi: model.valintaryhma.nimi
            };

            if(oid === "root"){
                Valintaryhma.insert(newValintaryhma, function(result) {
                    Treemodel.refresh();
                    $location.path("/");
                });
            } else {
                ChildValintaryhmas.insert({"parentOid": oid}, newValintaryhma, function(result){
                    Treemodel.refresh();
                    model.valintaryhma = result;
                });
            }
        };
    }

    return model;
});

function ValintaryhmaCreatorController($scope, $location, $routeParams, ValintaryhmaCreatorModel) {
    $scope.valintaryhmaOid = $routeParams.id;
    $scope.model = ValintaryhmaCreatorModel;
    $scope.model.refreshIfNeeded($scope.valintaryhmaOid);

    $scope.submit = function() {
        $scope.model.persistValintaryhma($scope.valintaryhmaOid);
    }

    $scope.cancel = function() {
        $location.path("/");
    }

}










app.factory('ValintaryhmaChildrenModel', function($resource, $location, $routeParams, Hakukohde, Valintaryhma, ChildValintaryhmas, ChildHakukohdes ) {

    var model = new function() {
        this.valintaryhma = {};
        this.childValintaryhmat = [];
        this.childHakukohteet = [];

        this.refresh = function(oid) {
            if(!oid) {
                model.valintaryhma = {};
                model.childValintaryhmat = [];
                model.childHakukohteet = [];
            } else {

                Valintaryhma.get({oid: oid}, function(result) {
                    model.valintaryhma = result;
                });
               
                ChildValintaryhmas.get({parentOid: oid}, function(result) {
                    model.childValintaryhmat = result;
                });

                ChildHakukohdes.get({oid: oid}, function(result) {
                    model.childHakukohteet = result;
                });

            }
        };

        this.refreshIfNeeded = function(oid) {
            if( oid !== model.valintaryhma.oid ) {
                this.refresh(oid);
            }
        };

        this.persistChildHakukohteet = function() {
            model.childHakukohteet.forEach(function(element, index, array) {
                Hakukohde.post(element, function(result) {
                    model.refresh(model.valintaryhma.oid);
                })
            });
        };

        this.addHakukohde = function(oid) {
            if(!model.valintaryhmaContainsHakukohde(oid)) {
                Hakukohde.get({oid: oid}, function(result) {
                    if(result) {
                        model.childHakukohteet.push(result);  
                        model.persistChildHakukohteet();  
                    }
                });
            }
        };

        this.valintaryhmaContainsHakukohde = function(hakukohdeOid) {

            var currentHakukohdeOids = [];
            model.childHakukohteet.forEach(function(element, index, array) {
                currentHakukohdeOids.push(element.oid);
            });

            var hakukohdeExistIndex = currentHakukohdeOids.indexOf(hakukohdeOid);
            if(hakukohdeExistIndex === -1) {
                return false;
            } else {
                return true;
            }
        };
    }

    return model;
});

function ValintaryhmaChildrenController($scope, $location, $routeParams, ValintaryhmaChildrenModel) {
    $scope.valintaryhmaOid = $routeParams.id;
    $scope.model = ValintaryhmaChildrenModel;
    $scope.model.refreshIfNeeded($scope.valintaryhmaOid);

    $scope.toValintaryhmaForm = function() {
        $location.path("/valintaryhma/" + $scope.valintaryhmaOid);
    };
}
