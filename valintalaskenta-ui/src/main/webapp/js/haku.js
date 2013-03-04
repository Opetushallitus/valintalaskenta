app.factory('HakuModel', function() {
    var model;
    model = new function() {
        this.hakuOid = {};
        this.haut = [];
        this.previous = {};

        this.init = function() {
            if(model.haut.length <= 0) {
                model.haut = [
                        {"oid": "oid1", "nimi": "Toisen asteen ammatillisen ja lukiokoulutuksen yhteishaku, kevät 2013"},
                        {"oid": "oid2", "nimi": "oid2"},
                        {"oid": "oid3", "nimi": "oid3"},
                        {"oid": "oid4", "nimi": "oid4"}
                    ];

                model.hakuOid = model.haut[0];
            }
        }
    };

    return model;
});

function HakuController($scope, $location, $routeParams, HakuModel) {
    $scope.model = HakuModel;
    HakuModel.init();
    $scope.$watch('model.hakuOid', function() {
        if(HakuModel.hakuOid !== HakuModel.previous) {
            HakuModel.previous = HakuModel.hakuOid;
            $location.path('/haku/' + HakuModel.hakuOid.oid + '/hakukohde/');
        }
    });
}