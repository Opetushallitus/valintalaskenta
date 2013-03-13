app.factory('HakuModel', function(Haku) {
    var model;
    model = new function() {
        this.hakuOid = {};
        this.haut = [];

        this.init = function(oid) {
            // Ladataan haut vain kerran. Haut tuskin muuttuvat kovinkaan usein.
            if(model.haut.length <= 0) {
                Haku.get({}, function(result) {
                    model.haut = result;
                    model.hakuOid = model.haut[0];
                    model.haut.forEach(function(haku){
                        if(haku.oid == oid) {
                            model.hakuOid = haku;
                        }
                    });
                });
            }
        }
    };

    return model;
});

function HakuController($scope, $location, $routeParams, HakuModel) {
    $scope.model = HakuModel;
    HakuModel.init($routeParams.hakuOid);

    $scope.$watch('model.hakuOid', function() {
        if(HakuModel.hakuOid.oid &&
            HakuModel.hakuOid.oid != "undefined" &&
            HakuModel.hakuOid.oid != $routeParams.hakuOid) {
            $location.path('/haku/' + HakuModel.hakuOid.oid + '/hakukohde/');
        }
    });
}