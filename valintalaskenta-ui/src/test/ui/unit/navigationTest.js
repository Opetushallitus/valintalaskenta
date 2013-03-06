'use strict';

describe('navigationController', function(){
    var scope, location, ctrl, routeParams;

    beforeEach( function() {
        scope = {};
        location = new function(){
            this.path = function() {
                return "/haku/oid1/hakukohde/oid2";
            }
        };
        routeParams = {"hakuOid": "oid1", "hakukohdeOid": "oid2"};
        ctrl = new navigationController(scope, location, routeParams);
    });


    it('should have hakuOid', function() {
        expect(scope.hakuOid).toBe("oid1");
    });

    it('should have hakukohdeOid', function() {
        expect(scope.hakukohdeOid).toBe("oid2");
    });

    it('navClass should be current', function() {
        expect(scope.navClass("haku", 1)).toBe("current");
    });

    it('navClass should be empty', function() {
        expect(scope.navClass("haku", 3)).toBe("");
    });
});
