app.factory('HakukohteetModel', function(Haku, allHakukohteet) {
    var model;
    model = new function(){
        
        this.hakukohteet = [];

        this.refresh = function(hakuOid) {
            model.hakuoid = hakuOid;

            allHakukohteet.get({}, function(result) {
                model.hakukohteet = result;
                //console.log(result);
            });
           

        };

        this.refreshIfNeeded = function(hakuOid) {
            model.refresh(hakuOid);
        };
    };

    return model;
});

function HakukohteetController($scope, $location, $routeParams, HakukohteetModel) {
   $scope.hakuOid = $routeParams.hakuOid;

   $scope.hakukohdeOid = $routeParams.hakukohdeOid;

   $scope.model = HakukohteetModel;
   $scope.model.refreshIfNeeded($routeParams.hakuOid);
}