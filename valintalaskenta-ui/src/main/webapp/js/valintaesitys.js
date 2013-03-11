
app.factory('ValintaesitysModel', function(Hakukohde, Haku) {

	var model = new function() {

		this.hakukohde = {};
		this.valintatapajonot = [];
		
		this.refresh = function(hakukohdeOid) {
			/*
			console.log("here");
			Haku.get({}, function(result) {
				console.log(result);
			});
			
			/*
			Haku.get({}, function(result) {
				console.log(result);
				//var hakukohderesultOid = result[0].oid;
				/*
				//hakukohteen oid saadaan joskus routeparametrien mukana jonkin kohteen tiedoilla
				Hakukohde.get({hakukohdeoid: hakukohderesultOid}, function(result) {
					model.hakukohde = result[0];
					model.valintatapajonot = model.hakukohde.valintatapajono;
				});
				
			});
			*/
			
		}
		

	};

	return model;
});


function ValintaesitysController($scope, $location, $routeParams, ValintaesitysModel) {
    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
    $scope.model = ValintaesitysModel;
    $scope.model.refresh($scope.hakukohdeOid);

}