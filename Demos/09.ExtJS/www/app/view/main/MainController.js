Ext.define('SecondDemo.view.main.MainController', {
    extend: 'Ext.app.ViewController',
    alias: 'controller.main',

    init: function () {
        // Role management demo
        var idAdmin = true;
        this.getView().items.each(function (item) {
            if (item.requireAdmin === true && !idAdmin) {
                item.disable();
            }
        });
    }
});