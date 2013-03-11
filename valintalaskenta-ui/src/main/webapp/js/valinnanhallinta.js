app.factory('ValinnanhallintaModel', function(tarjontaHakukohde) {

	var model = new function() {

		this.hakukohde = {};

		this.refresh = function(hakukohdeOid) {
			if( hakukohdeOid !== undefined) {
				tarjontaHakukohde.get({hakukohdeoid: hakukohdeOid}, function(result) {
					model.hakukohde = result;
				});
			}
		}

		this.refreshIfNeeded = function(hakukohdeOid) {

			if(model.hakukohde.oid !== hakukohdeOid) {
				model.refresh(hakukohdeOid);
			}
		}

	};



	return model;

});

function ValinnanhallintaController($scope, $location, $routeParams, ValinnanhallintaModel) {
	$scope.model = ValinnanhallintaModel;

	$scope.model.refreshIfNeeded($routeParams.hakukohdeOid);
}