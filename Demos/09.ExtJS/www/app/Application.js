/**
 * The main application class. An instance of this class is created by app.js when it
 * calls Ext.application(). This is the ideal place to handle application launch and
 * initialization details.
 */
Ext.define('SecondDemo.Application', {
    extend: 'Ext.app.Application',
    
    name: 'SecondDemo',

    requires: [
        'SecondDemo.view.main.Main'
    ],

    stores: [
        // TODO: add global / shared stores here
    ],
    
    launch: function () {
        Ext.widget('app-main');
    },

    onAppUpdate: function () {
        Ext.Msg.confirm('Application Update', 'This application has an update, reload?',
            function (choice) {
                if (choice === 'yes') {
                    window.location.reload();
                }
            }
        );
    }
});
