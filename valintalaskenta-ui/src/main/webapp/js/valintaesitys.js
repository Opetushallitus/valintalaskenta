
app.factory('ValintaesitysModel', function(Hakukohde, Haku) {

	var model = new function() {

		this.hakukohde = {};
		this.valintatapajonot = [];
		
		this.refresh = function(hakukohdeOid) {
			
			Hakukohde.get({hakukohdeoid: hakukohdeOid}, function(result) {
				model.valintatapajonot = result[0].valintatapajono;
			});
			
		}

		this.refreshIfNeeded = function(hakukohdeOid) {
			
			if( model.hakukohde.oid !== hakukohdeOid ) {
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