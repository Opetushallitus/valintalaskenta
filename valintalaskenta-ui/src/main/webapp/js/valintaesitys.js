app.factory('ValintaesitysModel', function(ValinnanvaiheListByHakukohde) {
	var model;
	model = new function() {

		this.hakukohdeOid = {};
		this.valinnanvaiheet = [];
		
		this.refresh = function(hakukohdeOid) {
            model.hakukohdeOid = hakukohdeOid;
			ValinnanvaiheListByHakukohde.get({hakukohdeoid: hakukohdeOid}, function(result) {
			    model.valinnanvaiheet = result;
			    console.log(model.valinnanvaiheet);
			});
			
		}

		this.refreshIfNeeded = function(hakukohdeOid) {
			
			if( model.hakukohdeOid != hakukohdeOid ) {
				model.refresh(hakukohdeOid);
			}
		}
		

	};

	return model;
});


function ValintaesitysController($scope, $location, $routeParams, ValintaesitysModel) {
    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
    $scope.model = ValintaesitysModel;
    $scope.model.refreshIfNeeded($scope.hakukohdeOid);

}