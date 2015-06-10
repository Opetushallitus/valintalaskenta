package fi.vm.sade.valintalaskenta.tulos.dao.util;

public class MongoMapReduceUtil {
    public static final String shallowCloneJs = "function () { function shallowClone(obj) { var copy; if (null == obj || 'object' != typeof obj || obj instanceof ObjectId || obj instanceof NumberLong || obj instanceof Date) return obj; if (obj instanceof Array) { copy = []; for (var i = 0, len = obj.length; i < len; i++) { copy[i] = obj[i]; } return copy; } if (obj instanceof Object) { copy = {}; for (var attr in obj) { if (obj.hasOwnProperty(attr)) copy[attr] = obj[attr]; } return copy; } throw new Error('Unable to copy obj'); } var copy = shallowClone(this);";
}
