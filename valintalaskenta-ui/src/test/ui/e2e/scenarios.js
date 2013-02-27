'use strict';

describe('E2E-TESTS', function() {
  describe('valintaperusteet', function() {


    
    beforeEach(function() {
      browser().navigateTo('../../html/index.html');
    });
  

    describe('JavaScript addition operator', function () {  
      it('adds two numbers together', function () {  
          expect(1 + 2).toEqual(3);  
      });
    });
    /*
    it('should redirect to rootpage', function() {
      browser().navigateTo('../../index.html');
      expect(browser().location().url()).toBe('/phones');
    });
    */ 
  });  
});

