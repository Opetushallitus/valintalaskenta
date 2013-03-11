app.factory('HakuModel', function(Haku) {
    var model;
    model = new function() {
        this.hakuOid = {};
        this.haut = [];

        this.refresh = function() {
            Haku.get({}, function(result) {
                model.haut = result;
            });
        }

        this.refreshIfNeeded = function(hakuOid) {
            if(model.hakuoid !== hakuOid) {
                model.refresh();
            }
        }

    };

    return model;
});

function HakuController($scope, $location, $routeParams, HakuModel) {
    $scope.model = HakuModel;
    $scope.model.refreshIfNeeded($routeParams.hakuOid);
    /*
    $scope.$watch('model.hakuOid', function() {
        if(HakuModel.hakuOid.oid != $routeParams.hakuOid) {
            $location.path('/haku/' + HakuModel.hakuOid.oid + '/hakukohde/');
        }
    });
    */
}