app.factory('HakukohteetModel', function() {
    var model;
    model = new function(){
        this.hakuoid;
        this.hakukohteet = [];

        function refresh(oid) {
            model.hakuoid = oid;

//            Pitäisi hakea tarjonnasta
            model.hakukohteet = [
                {"oid": "oid1", "nimi": "Sosiaali ja terveysalan perustutkinto, pk Stadia ammattiopisto, Vilppulankatu"},
                {"oid": "oid2", "nimi": "Sosiaali ja terveysalan perustutkinto, pk Stadia ammattiopisto, Vilppulankatu"},
                {"oid": "oid3", "nimi": "Sosiaali ja terveysalan perustutkinto, pk Stadia ammattiopisto, Vilppulankatu"},
                {"oid": "oid4", "nimi": "Sosiaali ja terveysalan perustutkinto, pk Stadia ammattiopisto, Vilppulankatu"} ];
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
   $scope.model = HakukohteetModel;
   $scope.model.refreshIfNeeded($routeParams.hakuOid);
}