
app.factory('ValintaesitysModel', function(Hakukohde) {

	var model = new function() {

		this.hakukohde = {};
		this.valintatapajonot = [];

		//hakukohteen oid saadaan joskus routeparametrien mukana jonkin kohteen tiedoilla
		this.refresh = function(hakukohdeOid) {
			

			
			Hakukohde.get({hakukohdeoid: "hakukohdeoid-isvjun"}, function(result) {
				model.hakukohde = result[0];


				model.valintatapajonot = model.hakukohde.valintatapajono;
				console.log(model.valintatapajonot[0].jarjestyskriteeritulokset[0]);
			});
			
		}
		

	};

	return model;
});


function ValintaesitysController($scope, $location, $routeParams, ValintaesitysModel) {

    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
    
    $scope.model = ValintaesitysModel;
    $scope.model.refresh($scope.hakukohdeOid);

    $scope.hakukohdeOid = $routeParams.hakukohdeOid;


}