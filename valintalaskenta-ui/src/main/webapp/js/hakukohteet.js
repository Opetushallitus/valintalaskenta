app.factory('HakukohteetModel', function() {
    var model;
    model = new function(){
        this.hakuoid;
        this.hakukohteet = [];

        function refresh(oid) {
            model.hakuoid = oid;

//            Pitäisi hakea tarjonnasta
            model.hakukohteet = [{"oid": "oid1"}, {"oid": "oid2"}, {"oid": "oid3"}, {"oid": "oid4"}];
        }

        this.refreshIfNeeded = function(oid) {
            if(oid != model.hakuoid) {
                refresh(oid);
            }
        };
    };

    return model;
});

function HakukohteetController($scope, $location, $routeParams, HakukohteetModel) {
   $scope.hakuOid = $routeParams.hakuOid;
   $scope.hakuOid = "syksynhaku";
   $scope.model = HakukohteetModel;

   $scope.model.refreshIfNeeded($scope.hakuOid);
}