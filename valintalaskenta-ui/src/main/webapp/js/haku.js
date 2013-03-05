app.factory('HakuModel', function() {
    var model;
    model = new function() {
        this.hakuOid = {};
        this.haut = [];

        this.init = function(oid) {
            if(model.haut.length <= 0) {
                model.haut = [
                        {"oid": "oid1", "nimi": "Toisen asteen ammatillisen ja lukiokoulutuksen yhteishaku, kevät 2013"},
                        {"oid": "oid2", "nimi": "oid2"},
                        {"oid": "oid3", "nimi": "oid3"},
                        {"oid": "oid4", "nimi": "oid4"}
                    ];
                model.hakuOid = model.haut[0];
                model.haut.forEach(function(haku){
                    if(haku.oid == oid) {
                        model.hakuOid = haku;
                    }
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
        if(HakuModel.hakuOid.oid != $routeParams.hakuOid) {
            $location.path('/haku/' + HakuModel.hakuOid.oid + '/hakukohde/');
        }
    });
}