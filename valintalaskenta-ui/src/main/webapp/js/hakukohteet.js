app.factory('HakukohteetModel', function(HakuHakukohteet) {
    var model;
    model = new function(){
        this.hakuoid;
        this.hakukohteet = [];

        function refresh(oid) {
            model.hakuoid = oid;
            HakuHakukohteet.get({parentOid: oid}, function(result) {
                model.hakukohteet = result;
            });
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
   $scope.hakukohdeOid = $routeParams.hakukohdeOid;
   $scope.model = HakukohteetModel;

   $scope.model.refreshIfNeeded($scope.hakuOid);
}