/**
 * Created with IntelliJ IDEA.
 * User: tommiha
 * Date: 3/13/13
 * Time: 1:59 PM
 * To change this template use File | Settings | File Templates.
 */

app.factory('HakukohdeModel', function(tarjontaHakukohde) {
    var model;
    model = new function() {

        this.hakukohde = {};

        this.refresh = function(hakukohdeOid) {
            if( hakukohdeOid !== "undefined") {
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

function HakukohdeController($scope, $location, $routeParams, HakukohdeModel) {
    $scope.model = HakukohdeModel;
    $scope.model.refreshIfNeeded($routeParams.hakukohdeOid);
}